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
(define-constant err-subscription-not-found (err u109))
(define-constant err-subscription-expired (err u110))
(define-constant err-subscription-already-exists (err u111))
(define-constant err-invalid-subscription-period (err u112))

(define-constant license-commercial u1)
(define-constant license-personal u2)
(define-constant license-editorial u3)
(define-constant license-exclusive u4)

;; data vars
(define-data-var next-license-id uint u1)
(define-data-var platform-fee-percentage uint u250)
(define-data-var next-subscription-id uint u1)

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

(define-map license-subscriptions
  uint
  {
    license-id: uint,
    creator: principal,
    subscription-price: uint,
    period-blocks: uint,
    max-subscribers: uint,
    current-subscribers: uint,
    is-active: bool,
    created-at: uint
  }
)

(define-map active-subscriptions
  {subscription-id: uint, subscriber: principal}
  {
    started-at: uint,
    expires-at: uint,
    auto-renewal: bool,
    total-periods-paid: uint
  }
)

(define-map subscription-payments
  {subscription-id: uint, subscriber: principal, payment-period: uint}
  {
    paid-at: uint,
    amount-paid: uint,
    period-start: uint,
    period-end: uint
  }
)

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

(define-public (create-subscription
  (license-id uint)
  (subscription-price uint)
  (period-blocks uint)
  (max-subscribers uint))
  (let
    (
      (license-data (unwrap! (map-get? licenses license-id) err-token-not-found))
      (license-owner (unwrap! (nft-get-owner? art-license license-id) err-token-not-found))
      (subscription-id (var-get next-subscription-id))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender license-owner) err-not-token-owner)
    (asserts! (> subscription-price u0) err-invalid-price)
    (asserts! (> period-blocks u1000) err-invalid-subscription-period)
    (asserts! (> max-subscribers u0) err-invalid-subscription-period)
    
    (map-set license-subscriptions subscription-id
      {
        license-id: license-id,
        creator: tx-sender,
        subscription-price: subscription-price,
        period-blocks: period-blocks,
        max-subscribers: max-subscribers,
        current-subscribers: u0,
        is-active: true,
        created-at: current-block
      }
    )
    
    (var-set next-subscription-id (+ subscription-id u1))
    (ok subscription-id)
  )
)

(define-public (subscribe-to-license
  (subscription-id uint)
  (auto-renewal bool))
  (let
    (
      (subscription-data (unwrap! (map-get? license-subscriptions subscription-id) err-subscription-not-found))
      (current-block stacks-block-height)
      (subscription-key {subscription-id: subscription-id, subscriber: tx-sender})
      (existing-subscription (map-get? active-subscriptions subscription-key))
      (price (get subscription-price subscription-data))
      (creator (get creator subscription-data))
      (platform-fee (/ (* price (var-get platform-fee-percentage)) u10000))
      (creator-payment (- price platform-fee))
      (expires-at (+ current-block (get period-blocks subscription-data)))
    )
    (asserts! (get is-active subscription-data) err-subscription-not-found)
    (asserts! (< (get current-subscribers subscription-data) (get max-subscribers subscription-data)) err-subscription-already-exists)
    (asserts! (is-none existing-subscription) err-subscription-already-exists)
    (asserts! (not (is-eq tx-sender creator)) err-owner-only)
    
    (try! (stx-transfer? creator-payment tx-sender creator))
    (try! (stx-transfer? platform-fee tx-sender contract-owner))
    
    (map-set active-subscriptions subscription-key
      {
        started-at: current-block,
        expires-at: expires-at,
        auto-renewal: auto-renewal,
        total-periods-paid: u1
      }
    )
    
    (map-set subscription-payments
      {subscription-id: subscription-id, subscriber: tx-sender, payment-period: u1}
      {
        paid-at: current-block,
        amount-paid: price,
        period-start: current-block,
        period-end: expires-at
      }
    )
    
    (map-set license-subscriptions subscription-id
      (merge subscription-data {current-subscribers: (+ (get current-subscribers subscription-data) u1)})
    )
    
    (update-creator-stats creator false creator-payment)
    (ok true)
  )
)

(define-public (renew-subscription (subscription-id uint))
  (let
    (
      (subscription-data (unwrap! (map-get? license-subscriptions subscription-id) err-subscription-not-found))
      (subscription-key {subscription-id: subscription-id, subscriber: tx-sender})
      (active-sub (unwrap! (map-get? active-subscriptions subscription-key) err-subscription-not-found))
      (current-block stacks-block-height)
      (price (get subscription-price subscription-data))
      (creator (get creator subscription-data))
      (platform-fee (/ (* price (var-get platform-fee-percentage)) u10000))
      (creator-payment (- price platform-fee))
      (new-expires-at (+ (get expires-at active-sub) (get period-blocks subscription-data)))
      (next-period (+ (get total-periods-paid active-sub) u1))
    )
    (asserts! (get is-active subscription-data) err-subscription-not-found)
    (asserts! (< current-block (get expires-at active-sub)) err-subscription-expired)
    
    (try! (stx-transfer? creator-payment tx-sender creator))
    (try! (stx-transfer? platform-fee tx-sender contract-owner))
    
    (map-set active-subscriptions subscription-key
      (merge active-sub 
        {
          expires-at: new-expires-at,
          total-periods-paid: next-period
        }
      )
    )
    
    (map-set subscription-payments
      {subscription-id: subscription-id, subscriber: tx-sender, payment-period: next-period}
      {
        paid-at: current-block,
        amount-paid: price,
        period-start: (get expires-at active-sub),
        period-end: new-expires-at
      }
    )
    
    (update-creator-stats creator false creator-payment)
    (ok true)
  )
)

(define-public (cancel-subscription (subscription-id uint))
  (let
    (
      (subscription-data (unwrap! (map-get? license-subscriptions subscription-id) err-subscription-not-found))
      (subscription-key {subscription-id: subscription-id, subscriber: tx-sender})
      (active-sub (unwrap! (map-get? active-subscriptions subscription-key) err-subscription-not-found))
    )
    (map-delete active-subscriptions subscription-key)
    
    (map-set license-subscriptions subscription-id
      (merge subscription-data 
        {current-subscribers: (if (> (get current-subscribers subscription-data) u0)
                                (- (get current-subscribers subscription-data) u1)
                                u0)
        }
      )
    )
    (ok true)
  )
)

(define-public (deactivate-subscription (subscription-id uint))
  (let
    (
      (subscription-data (unwrap! (map-get? license-subscriptions subscription-id) err-subscription-not-found))
    )
    (asserts! (is-eq tx-sender (get creator subscription-data)) err-not-token-owner)
    
    (map-set license-subscriptions subscription-id
      (merge subscription-data {is-active: false})
    )
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

(define-read-only (get-subscription (subscription-id uint))
  (map-get? license-subscriptions subscription-id)
)

(define-read-only (get-active-subscription (subscription-id uint) (subscriber principal))
  (map-get? active-subscriptions {subscription-id: subscription-id, subscriber: subscriber})
)

(define-read-only (is-subscription-active (subscription-id uint) (subscriber principal))
  (match (map-get? active-subscriptions {subscription-id: subscription-id, subscriber: subscriber})
    subscription-data (< stacks-block-height (get expires-at subscription-data))
    false
  )
)

(define-read-only (get-subscription-payment (subscription-id uint) (subscriber principal) (payment-period uint))
  (map-get? subscription-payments {subscription-id: subscription-id, subscriber: subscriber, payment-period: payment-period})
)

(define-read-only (get-next-subscription-id)
  (var-get next-subscription-id)
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