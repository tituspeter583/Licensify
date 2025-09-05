;; title: LicensifyBulkManager
;; version: 1.0.0
;; summary: Bulk operations manager for Licensify licenses
;; description: Enables batch operations for license management

;; Constants
(define-constant ERR_UNAUTHORIZED (err u400))
(define-constant ERR_INVALID_INPUT (err u401))
(define-constant ERR_BATCH_TOO_LARGE (err u402))
(define-constant ERR_LICENSE_NOT_FOUND (err u403))
(define-constant MAX_BATCH_SIZE u10)

;; Data vars
(define-data-var contract-owner principal tx-sender)
(define-data-var next-operation-id uint u1)

;; Data maps for tracking operations
(define-map bulk-operation-history
  { operation-id: uint }
  {
    operator: principal,
    operation-type: (string-ascii 20),
    license-count: uint,
    created-at: uint
  }
)

(define-map user-license-registry
  { owner: principal, license-id: uint }
  { registered-at: uint }
)

(define-map user-license-counts principal uint)

;; Register license creation (called after successful license creation)
(define-public (register-license-creation (license-id uint))
  (let (
    (current-count (default-to u0 (map-get? user-license-counts tx-sender)))
    (current-block stacks-block-height)
  )
    (map-set user-license-registry
      { owner: tx-sender, license-id: license-id }
      { registered-at: current-block }
    )
    (map-set user-license-counts tx-sender (+ current-count u1))
    (ok true)
  )
)

;; Create single license through bulk manager
(define-public (create-single-license 
  (artwork-hash (string-ascii 64))
  (title (string-ascii 100))
  (license-type uint)
  (usage-terms (string-ascii 500))
  (price uint)
  (duration-blocks uint))
  (let (
    (license-result (contract-call? .Licensify create-license artwork-hash title license-type usage-terms price duration-blocks))
  )
    (match license-result
      success-result 
        (let (
          (reg-result (register-license-creation success-result))
        )
          (ok success-result)
        )
      error-result (err error-result)
    )
  )
)

;; Update single license price through bulk manager
(define-public (update-single-price (license-id uint) (new-price uint))
  (contract-call? .Licensify update-license-price license-id new-price)
)

;; Deactivate single license through bulk manager
(define-public (deactivate-single-license (license-id uint))
  (contract-call? .Licensify deactivate-license license-id)
)

;; Start bulk operation tracking
(define-public (start-bulk-operation (operation-type (string-ascii 20)) (license-count uint))
  (let (
    (operation-id (var-get next-operation-id))
    (current-block stacks-block-height)
  )
    (asserts! (<= license-count MAX_BATCH_SIZE) ERR_BATCH_TOO_LARGE)
    (asserts! (> license-count u0) ERR_INVALID_INPUT)
    
    (map-set bulk-operation-history
      { operation-id: operation-id }
      {
        operator: tx-sender,
        operation-type: operation-type,
        license-count: license-count,
        created-at: current-block
      }
    )
    
    (var-set next-operation-id (+ operation-id u1))
    (ok operation-id)
  )
)

;; Check if user owns a license
(define-public (check-license-ownership (license-id uint))
  (match (contract-call? .Licensify get-license-owner license-id)
    owner-principal (ok (is-eq tx-sender owner-principal))
    (err ERR_LICENSE_NOT_FOUND)
  )
)

;; Read-only functions
(define-read-only (get-user-license-count (owner principal))
  (default-to u0 (map-get? user-license-counts owner))
)

(define-read-only (is-license-registered (owner principal) (license-id uint))
  (is-some (map-get? user-license-registry { owner: owner, license-id: license-id }))
)

(define-read-only (get-bulk-operation (operation-id uint))
  (map-get? bulk-operation-history { operation-id: operation-id })
)

(define-read-only (get-next-operation-id)
  (var-get next-operation-id)
)

(define-read-only (get-max-batch-size)
  MAX_BATCH_SIZE
)

(define-read-only (get-license-registry-entry (owner principal) (license-id uint))
  (map-get? user-license-registry { owner: owner, license-id: license-id })
)

;; Helper function to get contract owner
(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

;; Admin function to update contract owner
(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)
