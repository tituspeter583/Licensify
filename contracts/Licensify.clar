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
(define-constant err-dispute-not-found (err u113))
(define-constant err-dispute-already-exists (err u114))
(define-constant err-invalid-dispute-status (err u115))
(define-constant err-not-dispute-participant (err u116))
(define-constant err-arbitrator-not-found (err u117))
(define-constant err-already-voted (err u118))
(define-constant err-voting-period-ended (err u119))
(define-constant err-dispute-not-resolved (err u120))
(define-constant err-insufficient-arbitrators (err u121))
(define-constant err-arbitrator-exists (err u122))
(define-constant err-not-arbitrator (err u123))
(define-constant err-dispute-resolved (err u124))

(define-constant dispute-status-open u1)
(define-constant dispute-status-evidence u2)
(define-constant dispute-status-voting u3)
(define-constant dispute-status-resolved u4)

(define-constant dispute-type-violation u1)
(define-constant dispute-type-unauthorized-use u2)
(define-constant dispute-type-payment u3)
(define-constant dispute-type-quality u4)

(define-constant resolution-favor-creator u1)
(define-constant resolution-favor-buyer u2)
(define-constant resolution-partial-refund u3)

(define-constant license-commercial u1)
(define-constant license-personal u2)
(define-constant license-editorial u3)
(define-constant license-exclusive u4)

;; data vars
(define-data-var next-license-id uint u1)
(define-data-var platform-fee-percentage uint u250)
(define-data-var next-subscription-id uint u1)
(define-data-var next-dispute-id uint u1)
(define-data-var arbitrator-registration-fee uint u1000000)
(define-data-var min-arbitrators uint u3)
(define-data-var dispute-voting-period uint u1008)
(define-data-var dispute-evidence-period uint u144)
(define-data-var total-arbitrators uint u0)

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

(define-map license-disputes
  uint
  {
    license-id: uint,
    complainant: principal,
    respondent: principal,
    dispute-type: uint,
    description: (string-ascii 500),
    status: uint,
    created-at: uint,
    evidence-deadline: uint,
    voting-deadline: uint,
    escrow-amount: uint,
    final-resolution: uint,
    resolved-at: uint
  }
)

(define-map dispute-evidence
  {dispute-id: uint, evidence-id: uint}
  {
    submitter: principal,
    evidence-hash: (string-ascii 64),
    description: (string-ascii 300),
    submitted-at: uint
  }
)

(define-map dispute-arbitrators
  principal
  {
    registered-at: uint,
    total-disputes: uint,
    successful-resolutions: uint,
    reputation-score: uint,
    is-active: bool,
    stake-amount: uint
  }
)

(define-map dispute-votes
  {dispute-id: uint, arbitrator: principal}
  {
    vote: uint,
    reasoning: (string-ascii 200),
    voted-at: uint
  }
)

(define-map dispute-assignments
  {dispute-id: uint, arbitrator: principal}
  {
    assigned-at: uint,
    is-active: bool
  }
)

(define-map arbitrator-earnings principal uint)

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

(define-public (register-arbitrator)
  (let
    (
      (registration-fee (var-get arbitrator-registration-fee))
      (current-block stacks-block-height)
      (existing-arbitrator (map-get? dispute-arbitrators tx-sender))
    )
    (asserts! (is-none existing-arbitrator) err-arbitrator-exists)
    
    (try! (stx-transfer? registration-fee tx-sender contract-owner))
    
    (map-set dispute-arbitrators tx-sender
      {
        registered-at: current-block,
        total-disputes: u0,
        successful-resolutions: u0,
        reputation-score: u100,
        is-active: true,
        stake-amount: registration-fee
      }
    )
    
    (var-set total-arbitrators (+ (var-get total-arbitrators) u1))
    (ok true)
  )
)

(define-public (create-dispute
  (license-id uint)
  (dispute-type uint)
  (description (string-ascii 500))
  (escrow-amount uint))
  (let
    (
      (license-data (unwrap! (map-get? licenses license-id) err-token-not-found))
      (purchase-key {license-id: license-id, buyer: tx-sender})
      (purchase-data (map-get? license-purchases purchase-key))
      (dispute-id (var-get next-dispute-id))
      (current-block stacks-block-height)
      (evidence-deadline (+ current-block (var-get dispute-evidence-period)))
      (voting-deadline (+ evidence-deadline (var-get dispute-voting-period)))
      (creator (get creator license-data))
    )
    (asserts! (>= (var-get total-arbitrators) (var-get min-arbitrators)) err-insufficient-arbitrators)
    (asserts! (or (is-eq dispute-type dispute-type-violation)
                  (is-eq dispute-type dispute-type-unauthorized-use)
                  (is-eq dispute-type dispute-type-payment)
                  (is-eq dispute-type dispute-type-quality)) err-invalid-dispute-status)
    (asserts! (or (is-some purchase-data) (is-eq tx-sender creator)) err-not-dispute-participant)
    (asserts! (> escrow-amount u0) err-invalid-price)
    
    (try! (stx-transfer? escrow-amount tx-sender (as-contract tx-sender)))
    
    (map-set license-disputes dispute-id
      {
        license-id: license-id,
        complainant: tx-sender,
        respondent: (if (is-eq tx-sender creator) 
                      contract-owner
                      creator),
        dispute-type: dispute-type,
        description: description,
        status: dispute-status-open,
        created-at: current-block,
        evidence-deadline: evidence-deadline,
        voting-deadline: voting-deadline,
        escrow-amount: escrow-amount,
        final-resolution: u0,
        resolved-at: u0
      }
    )
    
    (var-set next-dispute-id (+ dispute-id u1))
    (ok dispute-id)
  )
)

(define-public (submit-evidence
  (dispute-id uint)
  (evidence-hash (string-ascii 64))
  (description (string-ascii 300)))
  (let
    (
      (dispute-data (unwrap! (map-get? license-disputes dispute-id) err-dispute-not-found))
      (current-block stacks-block-height)
      (evidence-id (+ (get created-at dispute-data) current-block))
    )
    (asserts! (< current-block (get evidence-deadline dispute-data)) err-voting-period-ended)
    (asserts! (or (is-eq tx-sender (get complainant dispute-data))
                  (is-eq tx-sender (get respondent dispute-data))) err-not-dispute-participant)
    (asserts! (is-eq (get status dispute-data) dispute-status-open) err-invalid-dispute-status)
    
    (map-set dispute-evidence
      {dispute-id: dispute-id, evidence-id: evidence-id}
      {
        submitter: tx-sender,
        evidence-hash: evidence-hash,
        description: description,
        submitted-at: current-block
      }
    )
    
    (ok evidence-id)
  )
)

(define-public (assign-arbitrators (dispute-id uint))
  (let
    (
      (dispute-data (unwrap! (map-get? license-disputes dispute-id) err-dispute-not-found))
      (current-block stacks-block-height)
      (selected-arbitrators (get-active-arbitrators u3))
    )
    (asserts! (>= current-block (get evidence-deadline dispute-data)) err-voting-period-ended)
    (asserts! (is-eq (get status dispute-data) dispute-status-open) err-invalid-dispute-status)
    (asserts! (>= (len selected-arbitrators) u3) err-insufficient-arbitrators)
    
    (map-set license-disputes dispute-id
      (merge dispute-data {status: dispute-status-voting})
    )
    
    (begin
      (assign-arbitrators-to-dispute dispute-id selected-arbitrators)
      (ok true)
    )
  )
)

(define-public (vote-on-dispute
  (dispute-id uint)
  (vote uint)
  (reasoning (string-ascii 200)))
  (let
    (
      (dispute-data (unwrap! (map-get? license-disputes dispute-id) err-dispute-not-found))
      (arbitrator-data (unwrap! (map-get? dispute-arbitrators tx-sender) err-arbitrator-not-found))
      (current-block stacks-block-height)
      (vote-key {dispute-id: dispute-id, arbitrator: tx-sender})
      (assignment-key {dispute-id: dispute-id, arbitrator: tx-sender})
      (assignment (unwrap! (map-get? dispute-assignments assignment-key) err-not-arbitrator))
    )
    (asserts! (< current-block (get voting-deadline dispute-data)) err-voting-period-ended)
    (asserts! (is-eq (get status dispute-data) dispute-status-voting) err-invalid-dispute-status)
    (asserts! (get is-active arbitrator-data) err-not-arbitrator)
    (asserts! (get is-active assignment) err-not-arbitrator)
    (asserts! (is-none (map-get? dispute-votes vote-key)) err-already-voted)
    (asserts! (or (is-eq vote resolution-favor-creator)
                  (is-eq vote resolution-favor-buyer)
                  (is-eq vote resolution-partial-refund)) err-invalid-dispute-status)
    
    (map-set dispute-votes vote-key
      {
        vote: vote,
        reasoning: reasoning,
        voted-at: current-block
      }
    )
    
    (ok true)
  )
)

(define-public (resolve-dispute (dispute-id uint))
  (let
    (
      (dispute-data (unwrap! (map-get? license-disputes dispute-id) err-dispute-not-found))
      (current-block stacks-block-height)
      (vote-result (calculate-dispute-outcome dispute-id))
      (escrow-amount (get escrow-amount dispute-data))
      (complainant (get complainant dispute-data))
      (respondent (get respondent dispute-data))
    )
    (asserts! (>= current-block (get voting-deadline dispute-data)) err-voting-period-ended)
    (asserts! (is-eq (get status dispute-data) dispute-status-voting) err-invalid-dispute-status)
    
    (map-set license-disputes dispute-id
      (merge dispute-data 
        {
          status: dispute-status-resolved,
          final-resolution: vote-result,
          resolved-at: current-block
        }
      )
    )
    
    (begin
      (execute-dispute-resolution dispute-id vote-result escrow-amount complainant respondent)
      (distribute-arbitrator-rewards dispute-id vote-result)
      (ok vote-result)
    )
  )
)

(define-public (deactivate-arbitrator)
  (let
    (
      (arbitrator-data (unwrap! (map-get? dispute-arbitrators tx-sender) err-arbitrator-not-found))
      (stake-amount (get stake-amount arbitrator-data))
    )
    (asserts! (get is-active arbitrator-data) err-not-arbitrator)
    
    (map-set dispute-arbitrators tx-sender
      (merge arbitrator-data {is-active: false})
    )
    
    (try! (as-contract (stx-transfer? stake-amount tx-sender tx-sender)))
    (var-set total-arbitrators (- (var-get total-arbitrators) u1))
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

(define-read-only (get-dispute (dispute-id uint))
  (map-get? license-disputes dispute-id)
)

(define-read-only (get-dispute-evidence (dispute-id uint) (evidence-id uint))
  (map-get? dispute-evidence {dispute-id: dispute-id, evidence-id: evidence-id})
)

(define-read-only (get-arbitrator-info (arbitrator principal))
  (map-get? dispute-arbitrators arbitrator)
)

(define-read-only (get-dispute-vote (dispute-id uint) (arbitrator principal))
  (map-get? dispute-votes {dispute-id: dispute-id, arbitrator: arbitrator})
)

(define-read-only (get-arbitrator-assignment (dispute-id uint) (arbitrator principal))
  (map-get? dispute-assignments {dispute-id: dispute-id, arbitrator: arbitrator})
)

(define-read-only (get-arbitrator-earnings (arbitrator principal))
  (default-to u0 (map-get? arbitrator-earnings arbitrator))
)

(define-read-only (get-total-arbitrators)
  (var-get total-arbitrators)
)

(define-read-only (get-next-dispute-id)
  (var-get next-dispute-id)
)

(define-read-only (get-dispute-settings)
  {
    min-arbitrators: (var-get min-arbitrators),
    voting-period: (var-get dispute-voting-period),
    evidence-period: (var-get dispute-evidence-period),
    registration-fee: (var-get arbitrator-registration-fee)
  }
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

(define-private (get-active-arbitrators (max-count uint))
  (list)
)

(define-private (assign-arbitrators-to-dispute (dispute-id uint) (arbitrators (list 100 principal)))
  true
)

(define-private (calculate-dispute-outcome (dispute-id uint))
  resolution-favor-creator
)

(define-private (execute-dispute-resolution 
  (dispute-id uint) 
  (resolution uint) 
  (escrow-amount uint) 
  (complainant principal) 
  (respondent principal))
  true
)

(define-private (distribute-arbitrator-rewards (dispute-id uint) (winning-vote uint))
  true
)


