;; TimeVault NFT - Stacks Layer 2 Bitcoin (Clarity 4)
;; Lock STX and mint proof NFT with enhanced Clarity 4 features

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-vault-owner (err u101))
(define-constant err-already-withdrawn (err u102))
(define-constant err-still-locked (err u103))
(define-constant err-invalid-unlock-time (err u104))
(define-constant err-no-funds (err u105))
(define-constant err-transfer-failed (err u106))
(define-constant err-invalid-amount (err u107))
(define-constant err-lock-too-long (err u108))

;; Max lock duration: 10 years in blocks (~52560 blocks per year)
(define-constant max-lock-duration u525600)

;; Data Variables
(define-data-var vault-count uint u0)
(define-data-var token-count uint u0)
(define-data-var total-locked uint u0)

;; NFT Definition
(define-non-fungible-token time-vault-proof uint)

;; Data Maps
(define-map vaults
    uint
    {
        owner: principal,
        amount: uint,
        unlock-block: uint,
        withdrawn: bool,
        created-at: uint
    }
)

(define-map token-to-vault uint uint)
(define-map token-metadata uint (string-ascii 256))
(define-map owner-vaults principal (list 100 uint))

;; Read-only functions

(define-read-only (get-last-token-id)
    (ok (var-get token-count))
)

(define-read-only (get-vault-count)
    (ok (var-get vault-count))
)

(define-read-only (get-total-locked)
    (ok (var-get total-locked))
)

(define-read-only (get-token-uri (token-id uint))
    (ok (map-get? token-metadata token-id))
)

(define-read-only (get-owner (token-id uint))
    (ok (nft-get-owner? time-vault-proof token-id))
)

(define-read-only (get-vault-info (vault-id uint))
    (match (map-get? vaults vault-id)
        vault (ok {
            owner: (get owner vault),
            amount: (get amount vault),
            unlock-block: (get unlock-block vault),
            withdrawn: (get withdrawn vault),
            created-at: (get created-at vault),
            is-unlocked: (>= block-height (get unlock-block vault)),
            blocks-remaining: (if (>= block-height (get unlock-block vault))
                u0
                (- (get unlock-block vault) block-height))
        })
        (err u404)
    )
)

(define-read-only (get-vault-by-token (token-id uint))
    (ok (map-get? token-to-vault token-id))
)

(define-read-only (get-owner-vaults (owner principal))
    (ok (default-to (list) (map-get? owner-vaults owner)))
)

;; Helper function to add vault to owner's list
(define-private (add-vault-to-owner (vault-id uint) (owner principal))
    (let (
        (current-vaults (default-to (list) (map-get? owner-vaults owner)))
    )
        (map-set owner-vaults owner (unwrap-panic (as-max-len? (append current-vaults vault-id) u100)))
        (ok true)
    )
)

;; Public functions

(define-public (deposit (unlock-block uint) (token-uri (string-ascii 256)))
    (let
        (
            (sender tx-sender)
            (new-vault-id (+ (var-get vault-count) u1))
            (new-token-id (+ (var-get token-count) u1))
        )
        ;; Validations
        (asserts! (> unlock-block block-height) err-invalid-unlock-time)
        (asserts! (<= unlock-block (+ block-height max-lock-duration)) err-lock-too-long)
        
        ;; Create vault with current block height
        (map-set vaults new-vault-id {
            owner: sender,
            amount: u0, ;; Will be set via post-condition
            unlock-block: unlock-block,
            withdrawn: false,
            created-at: block-height
        })
        
        ;; Mint NFT
        (try! (nft-mint? time-vault-proof new-token-id sender))
        
        ;; Set metadata
        (map-set token-metadata new-token-id token-uri)
        (map-set token-to-vault new-token-id new-vault-id)
        
        ;; Add vault to owner's list
        (try! (add-vault-to-owner new-vault-id sender))
        
        ;; Update counters
        (var-set vault-count new-vault-id)
        (var-set token-count new-token-id)
        
        (print {
            event: "vault-created",
            vault-id: new-vault-id,
            token-id: new-token-id,
            owner: sender,
            unlock-block: unlock-block,
            created-at: block-height
        })
        
        (ok new-vault-id)
    )
)

;; Separate function to deposit STX (called after vault creation)
(define-public (fund-vault (vault-id uint) (amount uint))
    (let
        (
            (vault (unwrap! (map-get? vaults vault-id) (err u404)))
        )
        ;; Validations
        (asserts! (is-eq tx-sender (get owner vault)) err-not-vault-owner)
        (asserts! (is-eq (get amount vault) u0) err-already-withdrawn)
        (asserts! (> amount u0) err-invalid-amount)
        
        ;; Transfer STX to contract
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        ;; Update vault with amount
        (map-set vaults vault-id (merge vault { amount: amount }))
        
        ;; Update total locked
        (var-set total-locked (+ (var-get total-locked) amount))
        
        (print {
            event: "vault-funded",
            vault-id: vault-id,
            amount: amount
        })
        
        (ok true)
    )
)

(define-public (withdraw (vault-id uint))
    (let
        (
            (vault (unwrap! (map-get? vaults vault-id) (err u404)))
            (amount (get amount vault))
        )
        ;; Validations
        (asserts! (is-eq tx-sender (get owner vault)) err-not-vault-owner)
        (asserts! (not (get withdrawn vault)) err-already-withdrawn)
        (asserts! (>= block-height (get unlock-block vault)) err-still-locked)
        (asserts! (> amount u0) err-no-funds)
        
        ;; Update vault
        (map-set vaults vault-id (merge vault {
            withdrawn: true,
            amount: u0
        }))
        
        ;; Update total locked
        (var-set total-locked (- (var-get total-locked) amount))
        
        ;; Transfer STX back to owner
        (match (as-contract (stx-transfer? amount tx-sender (get owner vault)))
            success (begin
                (print {
                    event: "vault-withdrawn",
                    vault-id: vault-id,
                    owner: tx-sender,
                    amount: amount,
                    withdrawn-at: block-height
                })
                (ok true)
            )
            error err-transfer-failed
        )
    )
)

;; Emergency withdraw (only if unlock time has passed)
(define-public (force-withdraw (vault-id uint))
    (let
        (
            (vault (unwrap! (map-get? vaults vault-id) (err u404)))
            (amount (get amount vault))
            ;; Add grace period of 1000 blocks after unlock
            (grace-unlock-block (+ (get unlock-block vault) u1000))
        )
        ;; Must be vault owner and past grace period
        (asserts! (is-eq tx-sender (get owner vault)) err-not-vault-owner)
        (asserts! (not (get withdrawn vault)) err-already-withdrawn)
        (asserts! (>= block-height grace-unlock-block) err-still-locked)
        (asserts! (> amount u0) err-no-funds)
        
        ;; Update vault
        (map-set vaults vault-id (merge vault {
            withdrawn: true,
            amount: u0
        }))
        
        ;; Update total locked
        (var-set total-locked (- (var-get total-locked) amount))
        
        ;; Transfer with higher priority
        (match (as-contract (stx-transfer? amount tx-sender (get owner vault)))
            success (begin
                (print {
                    event: "vault-force-withdrawn",
                    vault-id: vault-id,
                    owner: tx-sender,
                    amount: amount
                })
                (ok true)
            )
            error err-transfer-failed
        )
    )
)

;; NFT transfer function (Clarity 4 compatible)
(define-public (transfer (token-id uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) err-not-vault-owner)
        (try! (nft-transfer? time-vault-proof token-id sender recipient))
        
        ;; Update owner vaults lists
        (let (
            (vault-id (unwrap! (map-get? token-to-vault token-id) (err u404)))
            (vault (unwrap! (map-get? vaults vault-id) (err u404)))
        )
            ;; Update vault owner
            (map-set vaults vault-id (merge vault { owner: recipient }))
            
            ;; Add to recipient's vaults
            (try! (add-vault-to-owner vault-id recipient))
            
            (ok true)
        )
    )
)

;; Get contract statistics
(define-read-only (get-contract-stats)
    (ok {
        total-vaults: (var-get vault-count),
        total-tokens: (var-get token-count),
        total-locked: (var-get total-locked),
        current-block: block-height
    })
)
