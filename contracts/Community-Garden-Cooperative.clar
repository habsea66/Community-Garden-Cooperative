;; Scholarly Manuscript Registry: Decentralized academic paper submission and peer review platform
;; Enables researchers to submit manuscripts, reviewers to evaluate submissions, and editors to manage publications

(define-data-var chief-editor principal tx-sender)

(define-map manuscript-repository
  { manuscript-id: uint }
  {
    author: principal,
    submission-fee: uint,
    paper-title: (string-ascii 50),
    abstract-content: (string-ascii 500),
    review-period: uint,
    published: bool
  })

(define-map review-records
  { manuscript-id: uint, record-id: uint }
  {
    reviewer: principal,
    submission-date: uint,
    status: (string-ascii 20)
  })

(define-data-var next-manuscript-id uint u1)

(define-map record-tracker
  { manuscript-id: uint }
  { total-records: uint })

;; Submit a new manuscript for review
(define-public (submit-manuscript (title-input (string-ascii 50)) (abstract-input (string-ascii 500)) (period-input uint) (fee-input uint))
  (let
    (
      (manuscript-id (var-get next-manuscript-id))
      (record-id u0)
      (title title-input)
      (abstract abstract-input)
      (period period-input)
      (fee fee-input)
    )
    ;; Input validation
    (asserts! (> fee u0) (err u1))
    (asserts! (> (len title) u0) (err u5))
    (asserts! (> (len abstract) u0) (err u6))
    (asserts! (> period u0) (err u7))
    
    (map-set manuscript-repository
      { manuscript-id: manuscript-id }
      {
        author: tx-sender,
        submission-fee: fee,
        paper-title: title,
        abstract-content: abstract,
        review-period: period,
        published: false
      }
    )
    (map-set review-records
      { manuscript-id: manuscript-id, record-id: record-id }
      {
        reviewer: tx-sender,
        submission-date: manuscript-id,
        status: "submitted"
      }
    )
    (map-set record-tracker
      { manuscript-id: manuscript-id }
      { total-records: u1 }
    )
    (var-set next-manuscript-id (+ manuscript-id u1))
    (ok manuscript-id)
  ))

;; Assign reviewer to manuscript
(define-public (assign-reviewer (manuscript-id-input uint))
  (let
    (
      (manuscript-id manuscript-id-input)
      (manuscript-info (unwrap! (map-get? manuscript-repository { manuscript-id: manuscript-id }) (err u2)))
      (fee (get submission-fee manuscript-info))
      (author (get author manuscript-info))
      (record-data (default-to { total-records: u0 } (map-get? record-tracker { manuscript-id: manuscript-id })))
      (record-id (get total-records record-data))
      (new-record-id (+ record-id u1))
    )
    ;; Input validation
    (asserts! (> manuscript-id u0) (err u8))
    (asserts! (not (is-eq tx-sender author)) (err u3))
    
    (try! (stx-transfer? fee tx-sender author))
    (map-set review-records
      { manuscript-id: manuscript-id, record-id: record-id }
      {
        reviewer: tx-sender,
        submission-date: (var-get next-manuscript-id),
        status: "reviewing"
      }
    )
    (map-set record-tracker
      { manuscript-id: manuscript-id }
      { total-records: new-record-id }
    )
    (ok true)
  ))

;; Publish manuscript (chief editor only)
(define-public (publish-manuscript (manuscript-id-input uint))
  (let
    (
      (manuscript-id manuscript-id-input)
      (manuscript-info (unwrap! (map-get? manuscript-repository { manuscript-id: manuscript-id }) (err u2)))
      (record-data (default-to { total-records: u0 } (map-get? record-tracker { manuscript-id: manuscript-id })))
      (record-id (get total-records record-data))
      (new-record-id (+ record-id u1))
    )
    ;; Input validation
    (asserts! (> manuscript-id u0) (err u8))
    (asserts! (is-eq tx-sender (var-get chief-editor)) (err u4))
    
    (map-set manuscript-repository
      { manuscript-id: manuscript-id }
      (merge manuscript-info { published: true })
    )
    (map-set review-records
      { manuscript-id: manuscript-id, record-id: record-id }
      {
        reviewer: (get author manuscript-info),
        submission-date: (var-get next-manuscript-id),
        status: "published"
      }
    )
    (map-set record-tracker
      { manuscript-id: manuscript-id }
      { total-records: new-record-id }
    )
    (ok true)
  ))

;; Get manuscript details
(define-read-only (get-manuscript (manuscript-id uint))
  (map-get? manuscript-repository { manuscript-id: manuscript-id }))

;; Get review record entry
(define-read-only (get-review-record (manuscript-id uint) (record-id uint))
  (map-get? review-records { manuscript-id: manuscript-id, record-id: record-id }))

;; Get total review records for a manuscript
(define-read-only (get-record-count (manuscript-id uint))
  (let
    (
      (record-data (default-to { total-records: u0 } (map-get? record-tracker { manuscript-id: manuscript-id })))
    )
    (get total-records record-data)
  ))
