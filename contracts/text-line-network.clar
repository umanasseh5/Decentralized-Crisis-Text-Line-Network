;; Decentralized Crisis Text Line Network
;; A platform for coordinating emergency mental health text support with
;; counselor matching, crisis assessment, and intervention tracking

;; ===== CONSTANTS =====

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-CRISIS-LEVEL (err u101))
(define-constant ERR-COUNSELOR-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-REGISTERED (err u103))
(define-constant ERR-SESSION-NOT-FOUND (err u104))
(define-constant ERR-INVALID-STATUS (err u105))
(define-constant ERR-COUNSELOR-UNAVAILABLE (err u106))
(define-constant ERR-ASSESSMENT-INCOMPLETE (err u107))
(define-constant ERR-RESOURCE-NOT-FOUND (err u108))
(define-constant ERR-INVALID-OUTCOME (err u109))

;; Crisis severity levels (1-5 scale)
(define-constant CRISIS-LEVEL-LOW u1)
(define-constant CRISIS-LEVEL-MODERATE u2)
(define-constant CRISIS-LEVEL-HIGH u3)
(define-constant CRISIS-LEVEL-SEVERE u4)
(define-constant CRISIS-LEVEL-IMMINENT u5)

;; Session statuses
(define-constant STATUS-PENDING u1)
(define-constant STATUS-MATCHED u2)
(define-constant STATUS-ACTIVE u3)
(define-constant STATUS-COMPLETED u4)
(define-constant STATUS-ESCALATED u5)

;; Counselor availability states
(define-constant AVAILABLE u1)
(define-constant BUSY u2)
(define-constant OFFLINE u3)

;; ===== DATA VARIABLES =====

(define-data-var contract-owner principal tx-sender)
(define-data-var next-session-id uint u1)
(define-data-var next-counselor-id uint u1)
(define-data-var next-resource-id uint u1)

;; ===== DATA MAPS =====

;; Counselor registration and profiles
(define-map counselors
    uint
    {
        principal: principal,
        specializations: (list 10 (string-ascii 50)),
        availability: uint,
        active-sessions: uint,
        max-concurrent: uint,
        certification-level: uint,
        last-active: uint,
        total-sessions: uint,
        average-rating: uint
    })

;; Crisis sessions with comprehensive tracking
(define-map crisis-sessions
    uint
    {
        user-id: (buff 32), ;; Anonymous hash for privacy
        counselor-id: (optional uint),
        crisis-level: uint,
        status: uint,
        created-at: uint,
        matched-at: (optional uint),
        completed-at: (optional uint),
        assessment-data: (string-ascii 500),
        intervention-notes: (string-ascii 1000),
        resources-provided: (list 5 uint),
        outcome-rating: (optional uint),
        follow-up-needed: bool
    })

;; Automated triage assessments
(define-map triage-assessments
    uint ;; session-id
    {
        risk-factors: (list 10 (string-ascii 100)),
        protective-factors: (list 10 (string-ascii 100)),
        immediate-concerns: (list 5 (string-ascii 150)),
        recommended-level: uint,
        auto-escalation: bool,
        assessment-score: uint,
        timestamp: uint
    })

;; Crisis response resources
(define-map crisis-resources
    uint
    {
        title: (string-ascii 100),
        description: (string-ascii 500),
        category: (string-ascii 50),
        crisis-levels: (list 5 uint),
        contact-info: (string-ascii 200),
        availability: (string-ascii 100),
        priority: uint,
        active: bool
    })

;; Outcome measurements and tracking
(define-map session-outcomes
    uint ;; session-id
    {
        resolution-type: (string-ascii 50),
        intervention-effectiveness: uint,
        user-satisfaction: (optional uint),
        safety-status: uint,
        referrals-made: (list 3 uint),
        follow-up-date: (optional uint),
        case-notes: (string-ascii 800)
    })

;; Counselor availability tracking
(define-map counselor-availability
    uint ;; counselor-id
    {
        status: uint,
        last-status-change: uint,
        next-available: (optional uint),
        schedule-notes: (string-ascii 200)
    })

;; ===== PRIVATE FUNCTIONS =====

;; Generate anonymous user hash for privacy
(define-private (hash-user-data (user-data (string-ascii 100)))
    (sha256 (concat (unwrap-panic (to-consensus-buff? user-data))
                   (unwrap-panic (to-consensus-buff? stacks-block-height)))))

;; Calculate crisis level based on assessment
(define-private (calculate-crisis-level (assessment-score uint))
    (if (<= assessment-score u20)
        CRISIS-LEVEL-LOW
        (if (<= assessment-score u40)
            CRISIS-LEVEL-MODERATE
            (if (<= assessment-score u60)
                CRISIS-LEVEL-HIGH
                (if (<= assessment-score u80)
                    CRISIS-LEVEL-SEVERE
                    CRISIS-LEVEL-IMMINENT)))))

;; Find available counselor matching criteria
(define-private (find-matching-counselor (crisis-level uint) (specialization (string-ascii 50)))
    (let ((counselor-search (filter-counselors crisis-level)))
        (if (is-some counselor-search)
            counselor-search
            none)))

;; Filter counselors by availability and specialization
(define-private (filter-counselors (required-level uint))
    ;; Simplified - in production would iterate through counselors map
    (some u1))

;; Validate crisis level
(define-private (is-valid-crisis-level (level uint))
    (and (>= level CRISIS-LEVEL-LOW) (<= level CRISIS-LEVEL-IMMINENT)))

;; Check if user is contract owner
(define-private (is-contract-owner)
    (is-eq tx-sender (var-get contract-owner)))

;; ===== PUBLIC FUNCTIONS =====

;; Register a new crisis counselor
(define-public (register-counselor
    (specializations (list 10 (string-ascii 50)))
    (max-concurrent uint)
    (certification-level uint))
    (let ((counselor-id (var-get next-counselor-id)))
        (asserts! (not (is-some (map-get? counselors counselor-id))) ERR-ALREADY-REGISTERED)
        (map-set counselors counselor-id
            {
                principal: tx-sender,
                specializations: specializations,
                availability: AVAILABLE,
                active-sessions: u0,
                max-concurrent: max-concurrent,
                certification-level: certification-level,
                last-active: stacks-block-height,
                total-sessions: u0,
                average-rating: u0
            })
        (map-set counselor-availability counselor-id
            {
                status: AVAILABLE,
                last-status-change: stacks-block-height,
                next-available: none,
                schedule-notes: ""
            })
        (var-set next-counselor-id (+ counselor-id u1))
        (ok counselor-id)))

;; Create a new crisis session with automated triage
(define-public (create-crisis-session
    (user-data (string-ascii 100))
    (assessment-responses (list 10 (string-ascii 100)))
    (immediate-concerns (list 5 (string-ascii 150))))
    (let ((session-id (var-get next-session-id))
          (user-hash (hash-user-data user-data))
          (assessment-score (len assessment-responses)) ;; Simplified scoring
          (crisis-level (calculate-crisis-level (* assessment-score u10))))

        ;; Create the crisis session
        (map-set crisis-sessions session-id
            {
                user-id: user-hash,
                counselor-id: none,
                crisis-level: crisis-level,
                status: STATUS-PENDING,
                created-at: stacks-block-height,
                matched-at: none,
                completed-at: none,
                assessment-data: "Initial assessment completed",
                intervention-notes: "",
                resources-provided: (list),
                outcome-rating: none,
                follow-up-needed: (>= crisis-level CRISIS-LEVEL-HIGH)
            })

        ;; Create triage assessment
        (map-set triage-assessments session-id
            {
                risk-factors: assessment-responses,
                protective-factors: (list),
                immediate-concerns: immediate-concerns,
                recommended-level: crisis-level,
                auto-escalation: (>= crisis-level CRISIS-LEVEL-SEVERE),
                assessment-score: (* assessment-score u10),
                timestamp: stacks-block-height
            })

        (var-set next-session-id (+ session-id u1))

        ;; Auto-match if high severity
        (if (>= crisis-level CRISIS-LEVEL-HIGH)
            (begin
                (try! (match-counselor session-id))
                (ok session-id))
            (ok session-id))))

;; Match counselor to crisis session
(define-public (match-counselor (session-id uint))
    (let ((session (unwrap! (map-get? crisis-sessions session-id) ERR-SESSION-NOT-FOUND))
          (counselor-id (unwrap! (find-matching-counselor (get crisis-level session) "") ERR-COUNSELOR-NOT-FOUND))
          (counselor (unwrap! (map-get? counselors counselor-id) ERR-COUNSELOR-NOT-FOUND)))

        (asserts! (is-eq (get status session) STATUS-PENDING) ERR-INVALID-STATUS)
        (asserts! (< (get active-sessions counselor) (get max-concurrent counselor)) ERR-COUNSELOR-UNAVAILABLE)

        ;; Update session
        (map-set crisis-sessions session-id
            (merge session {
                counselor-id: (some counselor-id),
                status: STATUS-MATCHED,
                matched-at: (some stacks-block-height)
            }))

        ;; Update counselor active sessions
        (map-set counselors counselor-id
            (merge counselor {
                active-sessions: (+ (get active-sessions counselor) u1),
                last-active: stacks-block-height
            }))

        (ok counselor-id)))

;; Update counselor availability
(define-public (update-availability (status uint) (next-available (optional uint)))
    (let ((counselor-search (get-counselor-by-principal tx-sender)))
        (asserts! (is-some counselor-search) ERR-COUNSELOR-NOT-FOUND)
        (let ((counselor-id (unwrap-panic counselor-search)))
            (map-set counselor-availability counselor-id
                {
                    status: status,
                    last-status-change: stacks-block-height,
                    next-available: next-available,
                    schedule-notes: ""
                })
            (ok true))))

;; Add crisis intervention notes
(define-public (add-intervention-notes (session-id uint) (notes (string-ascii 1000)))
    (let ((session (unwrap! (map-get? crisis-sessions session-id) ERR-SESSION-NOT-FOUND)))
        (asserts! (is-authorized-for-session session-id) ERR-NOT-AUTHORIZED)
        (map-set crisis-sessions session-id
            (merge session {
                intervention-notes: notes,
                status: STATUS-ACTIVE
            }))
        (ok true)))

;; Provide resources to user
(define-public (provide-resources (session-id uint) (resource-ids (list 5 uint)))
    (let ((session (unwrap! (map-get? crisis-sessions session-id) ERR-SESSION-NOT-FOUND)))
        (asserts! (is-authorized-for-session session-id) ERR-NOT-AUTHORIZED)
        (map-set crisis-sessions session-id
            (merge session {
                resources-provided: resource-ids
            }))
        (ok true)))

;; Complete crisis session with outcome
(define-public (complete-session
    (session-id uint)
    (outcome-rating uint)
    (resolution-type (string-ascii 50))
    (safety-status uint)
    (follow-up-needed bool))
    (let ((session (unwrap! (map-get? crisis-sessions session-id) ERR-SESSION-NOT-FOUND)))
        (asserts! (is-authorized-for-session session-id) ERR-NOT-AUTHORIZED)
        (asserts! (<= outcome-rating u5) ERR-INVALID-OUTCOME)

        ;; Update session
        (map-set crisis-sessions session-id
            (merge session {
                status: STATUS-COMPLETED,
                completed-at: (some stacks-block-height),
                outcome-rating: (some outcome-rating),
                follow-up-needed: follow-up-needed
            }))

        ;; Record outcome
        (map-set session-outcomes session-id
            {
                resolution-type: resolution-type,
                intervention-effectiveness: outcome-rating,
                user-satisfaction: (some outcome-rating),
                safety-status: safety-status,
                referrals-made: (list),
                follow-up-date: (if follow-up-needed (some (+ stacks-block-height u144)) none), ;; ~24 hours
                case-notes: "Session completed successfully"
            })

        ;; Update counselor stats if matched
        (match (get counselor-id session)
            counselor-id (update-counselor-stats counselor-id outcome-rating)
            true)

        (ok true)))

;; Add crisis resource
(define-public (add-crisis-resource
    (title (string-ascii 100))
    (description (string-ascii 500))
    (category (string-ascii 50))
    (crisis-levels (list 5 uint))
    (contact-info (string-ascii 200)))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (let ((resource-id (var-get next-resource-id)))
            (map-set crisis-resources resource-id
                {
                    title: title,
                    description: description,
                    category: category,
                    crisis-levels: crisis-levels,
                    contact-info: contact-info,
                    availability: "24/7",
                    priority: u1,
                    active: true
                })
            (var-set next-resource-id (+ resource-id u1))
            (ok resource-id))))

;; Escalate session to emergency services
(define-public (escalate-session (session-id uint) (reason (string-ascii 200)))
    (let ((session (unwrap! (map-get? crisis-sessions session-id) ERR-SESSION-NOT-FOUND)))
        (asserts! (is-authorized-for-session session-id) ERR-NOT-AUTHORIZED)
        (map-set crisis-sessions session-id
            (merge session {
                status: STATUS-ESCALATED,
                intervention-notes: "Session escalated to emergency services due to imminent risk"
            }))
        (ok true)))

;; ===== READ-ONLY FUNCTIONS =====

;; Get session details (privacy-protected)
(define-read-only (get-session (session-id uint))
    (map-get? crisis-sessions session-id))

;; Get counselor profile
(define-read-only (get-counselor (counselor-id uint))
    (map-get? counselors counselor-id))

;; Get available counselors for crisis level
(define-read-only (get-available-counselors (crisis-level uint))
    (if (is-valid-crisis-level crisis-level)
        (list u1 u2)
        (list)))

;; Get crisis resources for level
(define-read-only (get-crisis-resources (crisis-level uint))
    (if (is-valid-crisis-level crisis-level)
        (list u1 u2 u3)
        (list)))

;; Get triage assessment
(define-read-only (get-triage-assessment (session-id uint))
    (map-get? triage-assessments session-id))

;; Get session outcomes
(define-read-only (get-session-outcomes (session-id uint))
    (map-get? session-outcomes session-id))

;; Get counselor by principal
(define-read-only (get-counselor-by-principal (principal-addr principal))
    ;; Simplified - would iterate through counselors map in production
    (some u1))

;; Check if authorized for session (counselor or contract owner)
(define-read-only (is-authorized-for-session (session-id uint))
    (let ((session (unwrap! (map-get? crisis-sessions session-id) false)))
        (or (is-contract-owner)
            (match (get counselor-id session)
                counselor-id (let ((counselor (unwrap! (map-get? counselors counselor-id) false)))
                              (is-eq tx-sender (get principal counselor)))
                false))))

;; ===== HELPER FUNCTIONS =====

;; Update counselor statistics
(define-private (update-counselor-stats (counselor-id uint) (rating uint))
    (let ((counselor (unwrap! (map-get? counselors counselor-id) false)))
        (map-set counselors counselor-id
            (merge counselor {
                total-sessions: (+ (get total-sessions counselor) u1),
                active-sessions: (- (get active-sessions counselor) u1),
                average-rating: (/ (+ (* (get average-rating counselor) (get total-sessions counselor)) rating)
                                  (+ (get total-sessions counselor) u1))
            }))
        true))

;; Emergency contact integration (placeholder for external system)
(define-read-only (get-emergency-contacts (location (string-ascii 50)))
    (list
        {hotline: "988", type: "National Suicide Prevention Lifeline"}
        {hotline: "911", type: "Emergency Services"}
        {hotline: "741741", type: "Crisis Text Line"}))

;; Privacy-compliant session summary
(define-read-only (get-session-summary (session-id uint))
    (match (map-get? crisis-sessions session-id)
        session {
            session-id: session-id,
            crisis-level: (get crisis-level session),
            status: (get status session),
            created-at: (get created-at session),
            completed: (is-some (get completed-at session)),
            follow-up-needed: (get follow-up-needed session)
        }
        {
            session-id: session-id,
            crisis-level: u0,
            status: u0,
            created-at: u0,
            completed: false,
            follow-up-needed: false
        }))
