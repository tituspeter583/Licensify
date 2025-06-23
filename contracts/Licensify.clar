;; title: Licensify
;; version: 1.0.0
;; summary: Art License NFT Registry for tracking image licensing with usage terms
;; description: A decentralized registry for managing art licenses as NFTs with customizable usage terms

;; traits
(define-trait license-trait
  (
    (transfer (uint principal principal) (response bool uint))
    (get-owner (uint) (response (optional principal) uint))
  )
)

;; token definitions
(define-non-fungible-token art-license uint)

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-token-exists (err u102))
(define-constant err-token-not-found (err u103))
(define-constant err-invalid-license-type (err u104))
(define-constant err-license-expired (err u105))
(define-constant err-unauthorized-usage (err u106))
(define-constant err-invalid-price (err u107))
(define-constant err-payment-failed (err u108))

(define-constant license-commercial u1)
(define-constant license-personal u2)
(define-constant license-editorial u3)
(define-constant license-exclusive u4)

;; data vars
(define-data-var next-license-id uint u1)
(define-data-var platform-fee-percentage uint u250)

;; data maps
(define-map licenses
  uint
  {
    creator: principal,
    artwork-hash: (string-ascii 64),
    title: (string-ascii 100),
    license-type: uint,
    usage-terms: (string-ascii 500),
    price: uint,
    duration-blocks: uint,
    created-at: uint,
    is-active: bool
  }
)

(define-map license-purchases
  {license-id: uint, buyer: principal}
  {
    purchased-at: uint,
    expires-at: uint,
    usage-granted: (string-ascii 200)
  }
)

(define-map creator-stats
  principal
  {
    total-licenses: uint,
    total-revenue: uint,
    active-licenses: uint
  }
)

(define-map license-usage-log
  {license-id: uint, usage-id: uint}
  {
    user: principal,
    usage-type: (string-ascii 50),
    timestamp: uint,
    metadata: (string-ascii 200)
  }
)

(define-map user-usage-counter principal uint)

;; public functions
(define-public (create-license 
  (artwork-hash (string-ascii 64))
  (title (string-ascii 100))
  (license-type uint)
  (usage-terms (string-ascii 500))
  (price uint)
  (duration-blocks uint))
  (let
    (
      (license-id (var-get next-license-id))
      (current-block stacks-block-height)
    )
    (asserts! (or (is-eq license-type license-commercial)
                  (is-eq license-type license-personal)
                  (is-eq license-type license-editorial)
                  (is-eq license-type license-exclusive)) err-invalid-license-type)
    (asserts! (> price u0) err-invalid-price)
    
    (try! (nft-mint? art-license license-id tx-sender))
    
    (map-set licenses license-id
      {
        creator: tx-sender,
        artwork-hash: artwork-hash,
        title: title,
        license-type: license-type,
        usage-terms: usage-terms,
        price: price,
        duration-blocks: duration-blocks,
        created-at: current-block,
        is-active: true
      }
    )
    
    (update-creator-stats tx-sender true u0)
    (var-set next-license-id (+ license-id u1))
    (ok license-id)
  )
)

(define-public (purchase-license (license-id uint))
  (let
    (
      (license-data (unwrap! (map-get? licenses license-id) err-token-not-found))
      (current-block stacks-block-height)
      (expires-at (+ current-block (get duration-blocks license-data)))
      (price (get price license-data))
      (creator (get creator license-data))
      (platform-fee (/ (* price (var-get platform-fee-percentage)) u10000))
      (creator-payment (- price platform-fee))
    )
    (asserts! (get is-active license-data) err-token-not-found)
    (asserts! (not (is-eq tx-sender creator)) err-owner-only)
    
    (try! (stx-transfer? creator-payment tx-sender creator))
    (try! (stx-transfer? platform-fee tx-sender contract-owner))
    
    ;; (map-set license-purchases
    ;;   {license-id: license-id, buyer: tx-sender}
    ;;   {
    ;;     purchased-at: current-block,
    ;;     expires-at: expires-at,
    ;;     usage-granted: (get usage-terms license-data)
    ;;   }
    ;; )
    
    (update-creator-stats creator false creator-payment)
    (ok true)
  )
)

(define-public (log-usage 
  (license-id uint)
  (usage-type (string-ascii 50))
  (metadata (string-ascii 200)))
  (let
    (
      (purchase-key {license-id: license-id, buyer: tx-sender})
      (purchase-data (unwrap! (map-get? license-purchases purchase-key) err-unauthorized-usage))
      (current-block stacks-block-height)
      (usage-id (default-to u0 (map-get? user-usage-counter tx-sender)))
    )
    (asserts! (< current-block (get expires-at purchase-data)) err-license-expired)
    
    (map-set license-usage-log
      {license-id: license-id, usage-id: usage-id}
      {
        user: tx-sender,
        usage-type: usage-type,
        timestamp: current-block,
        metadata: metadata
      }
    )
    
    (map-set user-usage-counter tx-sender (+ usage-id u1))
    (ok usage-id)
  )
)

(define-public (transfer-license (license-id uint) (recipient principal))
  (let
    (
      (current-owner (unwrap! (nft-get-owner? art-license license-id) err-token-not-found))
    )
    (asserts! (is-eq tx-sender current-owner) err-not-token-owner)
    (try! (nft-transfer? art-license license-id tx-sender recipient))
    (ok true)
  )
)

(define-public (deactivate-license (license-id uint))
  (let
    (
      (license-data (unwrap! (map-get? licenses license-id) err-token-not-found))
      (owner (unwrap! (nft-get-owner? art-license license-id) err-token-not-found))
    )
    (asserts! (is-eq tx-sender owner) err-not-token-owner)
    
    (map-set licenses license-id
      (merge license-data {is-active: false})
    )
    
    (update-creator-stats (get creator license-data) false u0)
    (ok true)
  )
)

(define-public (update-license-price (license-id uint) (new-price uint))
  (let
    (
      (license-data (unwrap! (map-get? licenses license-id) err-token-not-found))
      (owner (unwrap! (nft-get-owner? art-license license-id) err-token-not-found))
    )
    (asserts! (is-eq tx-sender owner) err-not-token-owner)
    (asserts! (> new-price u0) err-invalid-price)
    
    (map-set licenses license-id
      (merge license-data {price: new-price})
    )
    (ok true)
  )
)

(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u1000) err-invalid-price)
    (var-set platform-fee-percentage new-fee)
    (ok true)
  )
)

;; read only functions
(define-read-only (get-license (license-id uint))
  (map-get? licenses license-id)
)

(define-read-only (get-license-owner (license-id uint))
  (nft-get-owner? art-license license-id)
)

(define-read-only (get-purchase-info (license-id uint) (buyer principal))
  (map-get? license-purchases {license-id: license-id, buyer: buyer})
)

(define-read-only (is-license-valid (license-id uint) (buyer principal))
  (match (map-get? license-purchases {license-id: license-id, buyer: buyer})
    purchase-data (< stacks-block-height (get expires-at purchase-data))
    false
  )
)

(define-read-only (get-creator-stats (creator principal))
  (default-to 
    {total-licenses: u0, total-revenue: u0, active-licenses: u0}
    (map-get? creator-stats creator)
  )
)

(define-read-only (get-usage-log (license-id uint) (usage-id uint))
  (map-get? license-usage-log {license-id: license-id, usage-id: usage-id})
)

(define-read-only (get-next-license-id)
  (var-get next-license-id)
)

(define-read-only (get-platform-fee)
  (var-get platform-fee-percentage)
)

(define-read-only (get-license-type-name (license-type uint))
  (if (is-eq license-type license-commercial)
    "Commercial"
    (if (is-eq license-type license-personal)
      "Personal"
      (if (is-eq license-type license-editorial)
        "Editorial"
        (if (is-eq license-type license-exclusive)
          "Exclusive"
          "Unknown"
        )
      )
    )
  )
)

;; private functions
(define-private (update-creator-stats (creator principal) (is-new-license bool) (revenue uint))
  (let
    (
      (current-stats (get-creator-stats creator))
      (new-total (if is-new-license (+ (get total-licenses current-stats) u1) (get total-licenses current-stats)))
      (new-revenue (+ (get total-revenue current-stats) revenue))
      (new-active (if is-new-license 
                    (+ (get active-licenses current-stats) u1)
                    (if (> revenue u0)
                      (get active-licenses current-stats)
                      (if (> (get active-licenses current-stats) u0)
                        (- (get active-licenses current-stats) u1)
                        u0
                      )
                    )
                  ))
    )
    (map-set creator-stats creator
      {
        total-licenses: new-total,
        total-revenue: new-revenue,
        active-licenses: new-active
      }
    )
  )
)