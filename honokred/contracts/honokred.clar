;; Decentralized Reputation System
;; Allows users to build, earn, and manage reputation across different domains
;; and applications while maintaining privacy and control over their reputation data

(define-map reputation-domains
  { domain-id: uint }
  {
    domain-title: (string-utf8 64),
    description: (string-utf8 256),
    domain-owner: principal,
    created-height: uint,
    is-active: bool,
    endorsement-weight: uint,       ;; Weight of endorsements (out of 100)
    activity-weight: uint,          ;; Weight of activity (out of 100)
    verification-weight: uint,      ;; Weight of verifications (out of 100)
    minimum-endorsements-required: uint ;; Minimum endorsements needed for reputation score
  })

(define-map user-reputation
  { domain-id: uint, principal-account: principal }
  {
    reputation-score: uint,         ;; 0-1000 score
    endorsement-count: uint,        ;; Number of endorsements received
    activity-count: uint,           ;; Number of recorded activities
    verification-tier: uint,        ;; 0-5 verification level
    total-weighted-score: uint,     ;; Sum of weighted component scores
    last-updated-height: uint,      ;; Block height of last update
    decay-rate: uint                ;; Rate at which reputation decays if inactive (per 1000 blocks)
  })

(define-map endorsement-records
  { domain-id: uint, endorser: principal, endorsee: principal }
  {
    endorsement-weight: uint,       ;; 1-10 weight of endorsement
    created-height: uint,           ;; When endorsement was given
    endorsement-note: (optional (string-utf8 140)),  ;; Optional message
    endorsement-tags: (list 5 (string-ascii 20)), ;; Categories being endorsed
    is-active: bool                 ;; Whether endorsement is active
  })

(define-map verification-records
  { domain-id: uint, principal-account: principal, verification-type: (string-ascii 32) }
  {
    verified-by: principal,         ;; Who performed verification
    verification-height: uint,      ;; When verification was done
    expiration-height: (optional uint), ;; When verification expires
    evidence-hash: (buff 32),       ;; Hash of verification evidence
    verification-tier: uint,        ;; 1-5 verification level
    is-active: bool                 ;; Whether verification is active
  })

(define-map activity-records
  { domain-id: uint, activity-id: uint }
  {
    principal-account: principal,   ;; User who performed activity
    activity-type: (string-ascii 32), ;; Type of activity
    created-height: uint,           ;; When activity was recorded
    points: uint,                   ;; Value of activity (domain-specific)
    data-hash: (buff 32),           ;; Hash of activity data
    is-verified: bool,              ;; Whether activity is verified
    verified-by-principal: (optional principal) ;; Who verified the activity
  })

(define-map delegated-verifiers
  { domain-id: uint, verifier-principal: principal }
  {
    verifier-title: (string-utf8 64),
    approved-by-principal: principal,
    approval-height: uint,
    verification-types: (list 10 (string-ascii 32)),
    is-active: bool
  })

(define-map delegation-records
  { domain-id: uint, delegating-principal: principal }
  {
    delegate-principal: principal,
    delegation-height: uint,
    delegation-expiry: (optional uint),
    is-active: bool
  })

(define-map privacy-settings
  { domain-id: uint, principal-account: principal }
  {
    score-is-public: bool,          ;; Whether score is publicly viewable
    endorsements-public: bool,      ;; Whether endorsements are publicly viewable
    activities-public: bool,        ;; Whether activities are publicly viewable
    verifications-public: bool,     ;; Whether verifications are publicly viewable
    authorized-viewers: (list 10 principal) ;; Principals authorized to view private data
  })

;; Next available IDs
(define-data-var next-domain-id uint u0)
(define-map activity-id-counter { domain-id: uint } { id: uint })

;; Define our own min function since it's not built-in
(define-private (get-min (a uint) (b uint))
  (if (<= a b) a b))

;; Define our own max function since we'll need it
(define-private (get-max (a uint) (b uint))
  (if (>= a b) a b))

;; Validate domain ID
(define-private (validate-domain-id (domain-id uint))
  (if (< domain-id (var-get next-domain-id))
      (ok domain-id)
      (err u"Invalid domain ID")))

;; Validate string-utf8-64
(define-private (validate-string-utf8-64 (val (string-utf8 64)))
  (if (> (len val) u0)
      (ok val)
      (err u"String cannot be empty")))

;; Validate string-utf8-256
(define-private (validate-string-utf8-256 (val (string-utf8 256)))
  (if (> (len val) u0)
      (ok val)
      (err u"String cannot be empty")))

;; Validate string-ascii-32
(define-private (validate-string-ascii-32 (val (string-ascii 32)))
  (if (> (len val) u0)
      (ok val)
      (err u"String cannot be empty")))

;; Validate weight (1-10)
(define-private (validate-endorsement-weight (endorsement-weight uint))
  (if (and (>= endorsement-weight u1) (<= endorsement-weight u10))
      (ok endorsement-weight)
      (err u"Weight must be between 1 and 10")))

;; Validate verification level (1-5)
(define-private (validate-verification-tier (verification-tier uint))
  (if (and (>= verification-tier u1) (<= verification-tier u5))
      (ok verification-tier)
      (err u"Level must be between 1 and 5")))

;; Check if user can view another user's private data
(define-private (can-view-private-data (domain-id uint) (owner principal) (viewer principal))
  (let ((validated-domain-id-resp (validate-domain-id domain-id)))
    (asserts! (is-ok validated-domain-id-resp) false)
    
    (if (is-eq owner viewer)
        ;; Owner can always view own data
        true
        ;; Check privacy settings
        (match (map-get? privacy-settings { domain-id: domain-id, principal-account: owner })
          settings (is-some (index-of? (get authorized-viewers settings) viewer))
          false
        )
    )
  ))

;; Create a new reputation domain
(define-public (create-reputation-domain
                (domain-title (string-utf8 64))
                (description (string-utf8 256))
                (endorsement-weight uint)
                (activity-weight uint)
                (verification-weight uint)
                (minimum-endorsements-required uint))
  (let
    ((domain-id (var-get next-domain-id))
     (validated-domain-title-resp (validate-string-utf8-64 domain-title))
     (validated-description-resp (validate-string-utf8-256 description)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-domain-title-resp)
              (err (unwrap-err! validated-domain-title-resp (err u"Invalid name"))))
    (asserts! (is-ok validated-description-resp)
              (err (unwrap-err! validated-description-resp (err u"Invalid description"))))
    (asserts! (< (+ (+ endorsement-weight activity-weight) verification-weight) u101)
              (err u"Weights must sum to 100 or less"))
    (asserts! (> minimum-endorsements-required u0) (err u"Minimum endorsements must be greater than 0"))
    
    ;; Create the domain
    (map-set reputation-domains
      { domain-id: domain-id }
      {
        domain-title: (unwrap-panic validated-domain-title-resp),
        description: (unwrap-panic validated-description-resp),
        domain-owner: tx-sender,
        created-height: block-height,
        is-active: true,
        endorsement-weight: endorsement-weight,
        activity-weight: activity-weight,
        verification-weight: verification-weight,
        minimum-endorsements-required: minimum-endorsements-required
      }
    )
    
    ;; Initialize activity counter
    (map-set activity-id-counter
      { domain-id: domain-id }
      { id: u0 }
    )
    
    ;; Increment domain ID counter
    (var-set next-domain-id (+ domain-id u1))
    
    (ok domain-id)
  ))

;; Endorse a user
(define-public (endorse-user
                (domain-id uint)
                (endorsee principal)
                (endorsement-weight uint)
                (endorsement-note (optional (string-utf8 140)))
                (endorsement-tags (list 5 (string-ascii 20))))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id))
     (validated-endorsement-weight-resp (validate-endorsement-weight endorsement-weight)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-domain-id-resp)
              (err (unwrap-err! validated-domain-id-resp (err u"Invalid domain ID"))))
    (asserts! (is-ok validated-endorsement-weight-resp)
              (err (unwrap-err! validated-endorsement-weight-resp (err u"Invalid weight"))))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (validated-endorsement-weight (unwrap-panic validated-endorsement-weight-resp))
          (domain (unwrap! (map-get? reputation-domains { domain-id: validated-domain-id })
                          (err u"Domain not found")))
          (existing-endorsement (map-get? endorsement-records
                                 { domain-id: validated-domain-id, endorser: tx-sender, endorsee: endorsee })))
      
      ;; Validate
      (asserts! (not (is-eq tx-sender endorsee)) (err u"Cannot endorse yourself"))
      (asserts! (get is-active domain) (err u"Domain not active"))
      
      ;; Create or update the endorsement
      (map-set endorsement-records
        { domain-id: validated-domain-id, endorser: tx-sender, endorsee: endorsee }
        {
          endorsement-weight: validated-endorsement-weight,
          created-height: block-height,
          endorsement-note: endorsement-note,
          endorsement-tags: endorsement-tags,
          is-active: true
        }
      )
      
      ;; Update endorsee's reputation if no previous endorsement
      (if (is-none existing-endorsement)
          (update-endorsement-count validated-domain-id endorsee u1)
          true)
      
      ;; Recalculate reputation score and return success
      (let ((score (calculate-reputation-score validated-domain-id endorsee)))
        (ok score))
    )
  ))

;; Private helper to update endorsement count
(define-private (update-endorsement-count (domain-id uint) (principal-account principal) (delta uint))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id)))
    
    (asserts! (is-ok validated-domain-id-resp) false)
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (reputation (default-to
                        {
                        reputation-score: u0,
                        endorsement-count: u0,
                        activity-count: u0,
                        verification-tier: u0,
                        total-weighted-score: u0,
                        last-updated-height: block-height,
                        decay-rate: u10  ;; Default 1% decay per 1000 blocks
                      }
                      (map-get? user-reputation { domain-id: validated-domain-id, principal-account: principal-account }))))
      
      (map-set user-reputation
        { domain-id: validated-domain-id, principal-account: principal-account }
        (merge reputation { endorsement-count: (+ (get endorsement-count reputation) delta) })
      )
      
      ;; Changed from (ok true) to just true since this is a private function
      true
    )
  ))

;; Remove an endorsement
(define-public (remove-endorsement (domain-id uint) (endorsee principal))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-domain-id-resp)
              (err (unwrap-err! validated-domain-id-resp (err u"Invalid domain ID"))))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (endorsement (unwrap! (map-get? endorsement-records
                                { domain-id: validated-domain-id, endorser: tx-sender, endorsee: endorsee })
                              (err u"Endorsement not found"))))
      
      ;; Update the endorsement
      (map-set endorsement-records
        { domain-id: validated-domain-id, endorser: tx-sender, endorsee: endorsee }
        (merge endorsement { is-active: false })
      )
      
      ;; Update endorsee's reputation (using directly)
      (update-endorsement-count validated-domain-id endorsee (- u0 u1))
      
      ;; Recalculate reputation score and return success
      (let ((score (calculate-reputation-score validated-domain-id endorsee)))
        (ok score))
    )
  ))

;; Record an activity for reputation
(define-public (record-activity
                (domain-id uint)
                (activity-type (string-ascii 32))
                (points uint)
                (data-hash (buff 32)))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id))
     (validated-activity-type-resp (validate-string-ascii-32 activity-type)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-domain-id-resp)
              (err (unwrap-err! validated-domain-id-resp (err u"Invalid domain ID"))))
    (asserts! (is-ok validated-activity-type-resp)
              (err (unwrap-err! validated-activity-type-resp (err u"Invalid activity type"))))
    (asserts! (> (len data-hash) u0) (err u"Data hash cannot be empty"))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (validated-activity-type (unwrap-panic validated-activity-type-resp))
          (domain (unwrap! (map-get? reputation-domains { domain-id: validated-domain-id })
                          (err u"Domain not found")))
          (activity-id-counter-val (unwrap! (map-get? activity-id-counter { domain-id: validated-domain-id })
                                   (err u"Counter not found")))
          (activity-id (get id activity-id-counter-val))
          (reputation (default-to
                        {
                        reputation-score: u0,
                        endorsement-count: u0,
                        activity-count: u0,
                        verification-tier: u0,
                        total-weighted-score: u0,
                        last-updated-height: block-height,
                        decay-rate: u10  ;; Default 1% decay per 1000 blocks
                      }
                      (map-get? user-reputation { domain-id: validated-domain-id, principal-account: tx-sender }))))
      
      ;; Validate
      (asserts! (get is-active domain) (err u"Domain not active"))
      
      ;; Record the activity
      (map-set activity-records
        { domain-id: validated-domain-id, activity-id: activity-id }
        {
          principal-account: tx-sender,
          activity-type: validated-activity-type,
          created-height: block-height,
          points: points,
          data-hash: data-hash,
          is-verified: false,
          verified-by-principal: none
        }
      )
      
      ;; Update activity counter
      (map-set activity-id-counter
        { domain-id: validated-domain-id }
        { id: (+ activity-id u1) }
      )
      
      ;; Update user's activity count
      (map-set user-reputation
        { domain-id: validated-domain-id, principal-account: tx-sender }
        (merge reputation { 
          activity-count: (+ (get activity-count reputation) u1),
          last-updated-height: block-height
        })
      )
      
      ;; Recalculate reputation score and return the activity ID
      (calculate-reputation-score validated-domain-id tx-sender)
      (ok activity-id)
    )
  ))

;; Verify an activity (by domain admin or delegated verifier)
(define-public (verify-activity
                (domain-id uint)
                (activity-id uint))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-domain-id-resp)
              (err (unwrap-err! validated-domain-id-resp (err u"Invalid domain ID"))))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (domain (unwrap! (map-get? reputation-domains { domain-id: validated-domain-id })
                          (err u"Domain not found")))
          (activity (unwrap! (map-get? activity-records
                             { domain-id: validated-domain-id, activity-id: activity-id })
                           (err u"Activity not found"))))
      
      ;; Validate
      (asserts! (or (is-eq tx-sender (get domain-owner domain))
                   (is-delegated-verifier validated-domain-id tx-sender))
                (err u"Not authorized to verify"))
      (asserts! (not (get is-verified activity)) (err u"Activity already verified"))
      
      ;; Update activity
      (map-set activity-records
        { domain-id: validated-domain-id, activity-id: activity-id }
        (merge activity { 
          is-verified: true,
          verified-by-principal: (some tx-sender)
        })
      )
      
      ;; Recalculate reputation score for the activity owner and return success
      (let ((score (calculate-reputation-score validated-domain-id (get principal-account activity))))
        (ok score))
    )
  ))

;; Check if principal is a delegated verifier
(define-private (is-delegated-verifier (domain-id uint) (verifier-principal principal))
  (let ((validated-domain-id-resp (validate-domain-id domain-id)))
    (if (is-ok validated-domain-id-resp)
        (let ((validated-domain-id (unwrap-panic validated-domain-id-resp)))
          (default-to
            false
            (get is-active (map-get? delegated-verifiers
                        { domain-id: validated-domain-id, verifier-principal: verifier-principal }))
          )
        )
        false
    )
  ))

;; Add verification for a user
(define-public (add-verification
    (domain-id uint)
    (principal-account principal)
    (verification-type (string-ascii 32))
    (evidence-hash (buff 32))
    (verification-tier uint)
    (expiration-height (optional uint)))
    (let ((validated-domain-id-resp (validate-domain-id domain-id))
          (validated-verification-type-resp (validate-string-ascii-32 verification-type))
          (validated-verification-tier-resp (validate-verification-tier verification-tier)))
        
        ;; Validate parameters
        (asserts! (is-ok validated-domain-id-resp)
                   (err (unwrap-err! validated-domain-id-resp (err u"Invalid domain ID"))))
        (asserts! (is-ok validated-verification-type-resp)
                   (err (unwrap-err! validated-verification-type-resp (err u"Invalid verification type"))))
        (asserts! (is-ok validated-verification-tier-resp)
                   (err (unwrap-err! validated-verification-tier-resp (err u"Invalid level"))))
        (asserts! (> (len evidence-hash) u0) (err u"Evidence hash cannot be empty"))
        
        (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
              (validated-verification-type (unwrap-panic validated-verification-type-resp))
              (validated-verification-tier (unwrap-panic validated-verification-tier-resp))
              (domain (unwrap! (map-get? reputation-domains { domain-id: validated-domain-id })
                              (err u"Domain not found")))
              (reputation (default-to {
                  reputation-score: u0,
                  endorsement-count: u0,
                  activity-count: u0,
                  verification-tier: u0,
                  total-weighted-score: u0,
                  last-updated-height: block-height,
                  decay-rate: u10
              } (map-get? user-reputation { domain-id: validated-domain-id, principal-account: principal-account })))
              (current-verification-tier (get verification-tier reputation)))
            
            ;; Validate
            (asserts! (or (is-eq tx-sender (get domain-owner domain))
                         (is-delegated-verifier validated-domain-id tx-sender))
                      (err u"Not authorized to verify"))
            
            ;; Add verification
            (map-set verification-records
                { domain-id: validated-domain-id, principal-account: principal-account, verification-type: validated-verification-type }
                {
                    verified-by: tx-sender,
                    verification-height: block-height,
                    expiration-height: expiration-height,
                    evidence-hash: evidence-hash,
                    verification-tier: validated-verification-tier,
                    is-active: true
                }
            )
            
            ;; Update user's verification level (take highest verification level)
            (map-set user-reputation
                { domain-id: validated-domain-id, principal-account: principal-account }
                (merge reputation { 
                    verification-tier: (get-max current-verification-tier validated-verification-tier),
                    last-updated-height: block-height
                })
            )
            
            ;; Recalculate reputation score and return success
            (let ((score (calculate-reputation-score validated-domain-id principal-account)))
                (ok score))
        )
    ))

(define-public (revoke-verification
    (domain-id uint)
    (principal-account principal)
    (verification-type (string-ascii 32)))
    (let ((validated-domain-id-resp (validate-domain-id domain-id))
          (validated-verification-type-resp (validate-string-ascii-32 verification-type)))
        
        ;; Validate parameters
        (asserts! (is-ok validated-domain-id-resp)
                   (err (unwrap-err! validated-domain-id-resp (err u"Invalid domain ID"))))
        (asserts! (is-ok validated-verification-type-resp)
                   (err (unwrap-err! validated-verification-type-resp (err u"Invalid verification type"))))
        
        (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
              (validated-verification-type (unwrap-panic validated-verification-type-resp))
              (verification (unwrap! (map-get? verification-records
                                     { domain-id: validated-domain-id, principal-account: principal-account, verification-type: validated-verification-type })
                                    (err u"Verification not found"))))
            
            ;; Validate
            (asserts! (is-eq tx-sender (get verified-by verification))
                       (err u"Only verifier can revoke"))
            
            ;; Update verification
            (map-set verification-records
                { domain-id: validated-domain-id, principal-account: principal-account, verification-type: validated-verification-type }
                (merge verification { is-active: false })
            )
            
            ;; Recalculate highest verification level
            (recalculate-verification-tier validated-domain-id principal-account)
            
            ;; Recalculate reputation score and return success
            (let ((score (calculate-reputation-score validated-domain-id principal-account)))
                (ok score))
        )
    ))

(define-private (recalculate-verification-tier 
    (domain-id uint) 
    (principal-account principal))
    (let ((validated-domain-id-resp (validate-domain-id domain-id)))
        (asserts! (is-ok validated-domain-id-resp) false)
        
        (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
              (reputation (default-to {
                  reputation-score: u0,
                  endorsement-count: u0,
                  activity-count: u0,
                  verification-tier: u0,
                  total-weighted-score: u0,
                  last-updated-height: block-height,
                  decay-rate: u10
              } (map-get? user-reputation { domain-id: validated-domain-id, principal-account: principal-account }))))
            
            ;; For this example, we'll just reset to level 0
            (map-set user-reputation
                { domain-id: validated-domain-id, principal-account: principal-account }
                (merge reputation { verification-tier: u0 })
            )
            
            true
        )
    ))

;; Calculate reputation score
(define-private (calculate-reputation-score (domain-id uint) (principal-account principal))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id)))
    
    (asserts! (is-ok validated-domain-id-resp) u0)
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (domain (unwrap-panic (map-get? reputation-domains { domain-id: validated-domain-id })))
          (reputation (unwrap-panic (map-get? user-reputation { domain-id: validated-domain-id, principal-account: principal-account })))
          
          ;; Calculate component scores
          (endorsement-score (calculate-endorsement-score validated-domain-id principal-account
                                                       (get endorsement-count reputation)))
          (activity-score (calculate-activity-score validated-domain-id principal-account
                                                 (get activity-count reputation)))
          (verification-score (calculate-verification-score validated-domain-id principal-account
                                                         (get verification-tier reputation)))
          
          ;; Calculate weighted scores
          (weighted-endorsement (/ (* endorsement-score (get endorsement-weight domain)) u100))
          (weighted-activity (/ (* activity-score (get activity-weight domain)) u100))
          (weighted-verification (/ (* verification-score (get verification-weight domain)) u100))
          
          ;; Calculate total score
          (total-weighted (+ (+ weighted-endorsement weighted-activity) weighted-verification))
          (decayed-score (apply-decay total-weighted reputation)))
        
        ;; Update reputation score
        (map-set user-reputation
          { domain-id: validated-domain-id, principal-account: principal-account }
          (merge reputation { 
            reputation-score: decayed-score,
            total-weighted-score: total-weighted,
            last-updated-height: block-height
          })
        )
        
        ;; Return the score directly, not a response
        decayed-score
    )
  ))

;; Calculate endorsement score component (0-1000)
(define-private (calculate-endorsement-score (domain-id uint) (principal-account principal) (endorsement-count uint))
  ;; In a real implementation, this would be a more complex calculation
  ;; For this example, we'll use a simple scaling function
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id)))
    
    (asserts! (is-ok validated-domain-id-resp) u0)
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (domain (unwrap-panic (map-get? reputation-domains { domain-id: validated-domain-id })))
          (min-required (get minimum-endorsements-required domain)))
        
        (if (< endorsement-count min-required)
            ;; Below minimum: score = (count / min-required) * 500
            (/ (* endorsement-count u500) min-required)
            ;; Above minimum: score = 500 + (count - min-required) * 50, max 1000
            (get-min u1000 (+ u500 (* (- endorsement-count min-required) u50)))
        )
    )
  ))

;; Calculate activity score component (0-1000)
(define-private (calculate-activity-score (domain-id uint) (principal-account principal) (activity-count uint))
  ;; Simplified calculation
  (let ((validated-domain-id-resp (validate-domain-id domain-id)))
    (if (is-ok validated-domain-id-resp)
        (get-min u1000 (* activity-count u100))
        u0
    )
  ))

;; Calculate verification score component (0-1000)
(define-private (calculate-verification-score (domain-id uint) (principal-account principal) (verification-tier uint))
  ;; Level 0-5 scaled to 0-1000
  (let ((validated-domain-id-resp (validate-domain-id domain-id)))
    (if (is-ok validated-domain-id-resp)
        (* verification-tier u200)
        u0
    )
  ))

;; Apply decay to reputation score based on last update time
(define-private (apply-decay (score uint) (reputation (tuple
                                           (reputation-score uint)
                                           (endorsement-count uint)
                                           (activity-count uint)
                                           (verification-tier uint)
                                           (total-weighted-score uint)
                                           (last-updated-height uint)
                                           (decay-rate uint))))
  (let
    ((blocks-since-update (- block-height (get last-updated-height reputation)))
     (decay-periods (/ blocks-since-update u1000))
     (decay-rate-val (get decay-rate reputation)))
    
    (if (or (is-eq decay-periods u0) (is-eq decay-rate-val u0))
        ;; No decay
        score
        ;; Apply decay: score * (1 - decay-rate/100)^decay-periods
        ;; Simplified calculation
        (- score (/ (* score (* decay-periods decay-rate-val)) u1000))
    )
  ))

;; Add a delegated verification provider
(define-public (add-verification-provider
                (domain-id uint)
                (verifier-principal principal)
                (verifier-title (string-utf8 64))
                (verification-types (list 10 (string-ascii 32))))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id))
     (validated-verifier-title-resp (validate-string-utf8-64 verifier-title)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-domain-id-resp)
              (err (unwrap-err! validated-domain-id-resp (err u"Invalid domain ID"))))
    (asserts! (is-ok validated-verifier-title-resp)
              (err (unwrap-err! validated-verifier-title-resp (err u"Invalid name"))))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (validated-verifier-title (unwrap-panic validated-verifier-title-resp))
          (domain (unwrap! (map-get? reputation-domains { domain-id: validated-domain-id })
                          (err u"Domain not found"))))
      
      ;; Validate
      (asserts! (is-eq tx-sender (get domain-owner domain)) (err u"Only domain admin can add providers"))
      
      ;; Add provider
      (map-set delegated-verifiers
        { domain-id: validated-domain-id, verifier-principal: verifier-principal }
        {
          verifier-title: validated-verifier-title,
          approved-by-principal: tx-sender,
          approval-height: block-height,
          verification-types: verification-types,
          is-active: true
        }
      )
      
      (ok true)
    )
  ))

;; Revoke a verification provider
(define-public (revoke-verification-provider (domain-id uint) (verifier-principal principal))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-domain-id-resp)
              (err (unwrap-err! validated-domain-id-resp (err u"Invalid domain ID"))))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (domain (unwrap! (map-get? reputation-domains { domain-id: validated-domain-id })
                          (err u"Domain not found")))
          (provider-data (unwrap! (map-get? delegated-verifiers
                                  { domain-id: validated-domain-id, verifier-principal: verifier-principal })
                                (err u"Provider not found"))))
      
      ;; Validate
      (asserts! (is-eq tx-sender (get domain-owner domain)) (err u"Only domain admin can revoke providers"))
      
      ;; Update provider
      (map-set delegated-verifiers
        { domain-id: validated-domain-id, verifier-principal: verifier-principal }
        (merge provider-data { is-active: false })
      )
      
      (ok true)
    )
  ))

;; Delegate reputation management to another principal
(define-public (delegate-reputation (domain-id uint) (delegate-principal principal) (delegation-expiry (optional uint)))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-domain-id-resp)
              (err (unwrap-err! validated-domain-id-resp (err u"Invalid domain ID"))))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp)))
      (map-set delegation-records
        { domain-id: validated-domain-id, delegating-principal: tx-sender }
        {
          delegate-principal: delegate-principal,
          delegation-height: block-height,
          delegation-expiry: delegation-expiry,
          is-active: true
        }
      )
      
      (ok true)
    )
  ))

;; Remove reputation delegation
(define-public (remove-delegation (domain-id uint))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-domain-id-resp)
              (err (unwrap-err! validated-domain-id-resp (err u"Invalid domain ID"))))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (delegation (unwrap! (map-get? delegation-records
                              { domain-id: validated-domain-id, delegating-principal: tx-sender })
                             (err u"Delegation not found"))))
      
      (map-set delegation-records
        { domain-id: validated-domain-id, delegating-principal: tx-sender }
        (merge delegation { is-active: false })
      )
      
      (ok true)
    )
  ))

;; Update privacy settings
(define-public (update-privacy-settings
                (domain-id uint)
                (score-is-public bool)
                (endorsements-public bool)
                (activities-public bool)
                (verifications-public bool)
                (authorized-viewers (list 10 principal)))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id)))
    
    ;; Validate parameters
    (asserts! (is-ok validated-domain-id-resp)
              (err (unwrap-err! validated-domain-id-resp (err u"Invalid domain ID"))))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp)))
      (map-set privacy-settings
        { domain-id: validated-domain-id, principal-account: tx-sender }
        {
          score-is-public: score-is-public,
          endorsements-public: endorsements-public,
          activities-public: activities-public,
          verifications-public: verifications-public,
          authorized-viewers: authorized-viewers
        }
      )
      
      (ok true)
    )
  ))

;; Read-only functions

;; Get domain details
(define-read-only (get-domain-details (domain-id uint))
  (let ((validated-domain-id-resp (validate-domain-id domain-id)))
    (asserts! (is-ok validated-domain-id-resp) (err u"Invalid domain ID"))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp)))
      (ok (unwrap! (map-get? reputation-domains { domain-id: validated-domain-id }) (err u"Domain not found")))
    )
  ))

;; Get user reputation score
(define-read-only (get-reputation-score (domain-id uint) (principal-account principal))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id)))
    
    (asserts! (is-ok validated-domain-id-resp) (err u"Invalid domain ID"))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (reputation (map-get? user-reputation { domain-id: validated-domain-id, principal-account: principal-account }))
          (privacy (map-get? privacy-settings { domain-id: validated-domain-id, principal-account: principal-account })))
      
      (if (is-none reputation)
          (err u"Reputation not found")
          (if (or (is-none privacy)
                  (get score-is-public (unwrap-panic privacy))
                  (can-view-private-data validated-domain-id principal-account tx-sender))
              (ok (get reputation-score (unwrap-panic reputation)))
              (err u"Not authorized to view score")
          )
      )
    )
  ))

;; Get user endorsements
(define-read-only (get-user-endorsements (domain-id uint) (principal-account principal))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id)))
    
    (asserts! (is-ok validated-domain-id-resp) (err u"Invalid domain ID"))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (privacy (map-get? privacy-settings { domain-id: validated-domain-id, principal-account: principal-account })))
      
      (if (or (is-none privacy)
              (get endorsements-public (unwrap-panic privacy))
              (can-view-private-data validated-domain-id principal-account tx-sender))
          (ok u"Endorsements would be returned here")
          (err u"Not authorized to view endorsements")
      )
    )
  ))

;; Get verification details
(define-read-only (get-verification (domain-id uint) (principal-account principal) (verification-type (string-ascii 32)))
  (let
    ((validated-domain-id-resp (validate-domain-id domain-id))
     (validated-verification-type-resp (validate-string-ascii-32 verification-type)))
    
    (asserts! (is-ok validated-domain-id-resp) (err u"Invalid domain ID"))
    (asserts! (is-ok validated-verification-type-resp) (err u"Invalid verification type"))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp))
          (validated-verification-type (unwrap-panic validated-verification-type-resp))
          (verification (map-get? verification-records { domain-id: validated-domain-id, principal-account: principal-account, verification-type: validated-verification-type }))
          (privacy (map-get? privacy-settings { domain-id: validated-domain-id, principal-account: principal-account })))
      
      (if (is-none verification)
          (err u"Verification not found")
          (if (or (is-none privacy)
                  (get verifications-public (unwrap-panic privacy))
                  (can-view-private-data validated-domain-id principal-account tx-sender))
              (ok (unwrap-panic verification))
              (err u"Not authorized to view verification")
          )
      )
    )
  ))

;; Check if a provider is authorized for verification
(define-read-only (is-provider-authorized (domain-id uint) (verifier-principal principal))
  (let ((validated-domain-id-resp (validate-domain-id domain-id)))
    (asserts! (is-ok validated-domain-id-resp) (err u"Invalid domain ID"))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp)))
      (ok (is-delegated-verifier validated-domain-id verifier-principal))
    )
  ))

;; Get privacy settings
(define-read-only (get-privacy-settings (domain-id uint) (principal-account principal))
  (let ((validated-domain-id-resp (validate-domain-id domain-id)))
    (asserts! (is-ok validated-domain-id-resp) (err u"Invalid domain ID"))
    
    (let ((validated-domain-id (unwrap-panic validated-domain-id-resp)))
      (ok (default-to
            {
              score-is-public: true,
              endorsements-public: true,
              activities-public: false,
              verifications-public: false,
              authorized-viewers: (list)
            }
            (map-get? privacy-settings { domain-id: validated-domain-id, principal-account: principal-account })
          )
      )
    )
  ))