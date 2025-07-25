;; ===========================================
;; CONSTANTS & ERROR CODES
;; ===========================================

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_GENERATOR (err u101))
(define-constant ERR_INSUFFICIENT_HUMIDITY (err u102))
(define-constant ERR_POOR_WATER_QUALITY (err u103))
(define-constant ERR_INVALID_COORDINATES (err u104))
(define-constant ERR_GENERATOR_OFFLINE (err u105))
(define-constant ERR_INSUFFICIENT_ENERGY (err u106))
(define-constant ERR_INVALID_MAINTENANCE (err u107))
(define-constant ERR_COMMUNITY_NOT_FOUND (err u108))

;; Minimum thresholds for operation
(define-constant MIN_HUMIDITY u30) ;; 30% minimum humidity
(define-constant MIN_WATER_QUALITY u80) ;; 80% quality score minimum
(define-constant MIN_ENERGY_LEVEL u20) ;; 20% battery minimum
(define-constant MAX_COORDINATE u180000) ;; Coordinate bounds (1.8 degrees * 100000)

;; ===========================================
;; UTILITY FUNCTIONS
;; ===========================================

;; Custom absolute value function for integers
(define-private (abs-int (n int))
  (if (>= n 0) n (- n))
)

;; Custom minimum function for uints
(define-private (min-uint (a uint) (b uint))
  (if (<= a b) a b)
)

;; ===========================================
;; DATA STRUCTURES
;; ===========================================

;; Water generator registration and status
(define-map water-generators
  { generator-id: uint }
  {
    owner: principal,
    location: { latitude: int, longitude: int },
    capacity-liters-per-day: uint,
    energy-source: (string-ascii 50), ;; "solar", "wind", "hybrid", "grid"
    installation-block: uint,
    status: (string-ascii 20), ;; "active", "maintenance", "offline"
    community-id: uint
  }
)

;; Atmospheric conditions tracking
(define-map atmospheric-conditions
  { generator-id: uint, timestamp: uint }
  {
    humidity-percent: uint,
    temperature-celsius: int,
    pressure-hpa: uint,
    wind-speed-kmh: uint,
    solar-irradiance: uint, ;; W/m^2 for solar energy calculation
    recorded-by: principal
  }
)

;; Water production and quality records
(define-map water-production
  { generator-id: uint, batch-id: uint }
  {
    volume-liters: uint,
    quality-score: uint, ;; 0-100 scale
    ph-level: uint, ;; pH * 10 (e.g., 70 = pH 7.0)
    tds-ppm: uint, ;; Total Dissolved Solids
    production-timestamp: uint,
    energy-consumed-kwh: uint,
    distribution-status: (string-ascii 20) ;; "pending", "distributed", "stored"
  }
)

;; Community registration and water needs
(define-map desert-communities
  { community-id: uint }
  {
    name: (string-ascii 100),
    location: { latitude: int, longitude: int },
    population: uint,
    daily-water-need-liters: uint,
    registered-by: principal,
    registration-block: uint,
    priority-level: uint ;; 1-5 scale, 5 being highest need
  }
)

;; Energy system integration
(define-map energy-systems
  { generator-id: uint }
  {
    solar-capacity-kw: uint,
    wind-capacity-kw: uint,
    battery-capacity-kwh: uint,
    current-battery-level: uint, ;; Percentage 0-100
    grid-connected: bool,
    last-maintenance: uint,
    efficiency-rating: uint ;; 0-100 scale
  }
)

;; Water distribution tracking
(define-map water-distribution
  { distribution-id: uint }
  {
    community-id: uint,
    generator-id: uint,
    volume-distributed: uint,
    distribution-date: uint,
    transport-method: (string-ascii 50),
    cost-per-liter: uint, ;; In microSTX
    verified-by: principal
  }
)

;; ===========================================
;; GENERATOR MANAGEMENT
;; ===========================================

;; Register a new atmospheric water generator
(define-public (register-generator
    (generator-id uint)
    (latitude int)
    (longitude int)
    (capacity uint)
    (energy-source (string-ascii 50))
    (community-id uint))
  (begin
    ;; Validate coordinates (basic bounds check)
    (asserts! (and (< (abs-int latitude) (to-int MAX_COORDINATE)) (< (abs-int longitude) (to-int MAX_COORDINATE))) ERR_INVALID_COORDINATES)
    (asserts! (> capacity u0) ERR_INVALID_GENERATOR)

    ;; Register generator
    (map-set water-generators
      { generator-id: generator-id }
      {
        owner: tx-sender,
        location: { latitude: latitude, longitude: longitude },
        capacity-liters-per-day: capacity,
        energy-source: energy-source,
        installation-block: stacks-block-height,
        status: "active",
        community-id: community-id
      }
    )

    ;; Initialize energy system
    (map-set energy-systems
      { generator-id: generator-id }
      {
        solar-capacity-kw: u0,
        wind-capacity-kw: u0,
        battery-capacity-kwh: u100, ;; Default 100kWh
        current-battery-level: u100,
        grid-connected: false,
        last-maintenance: stacks-block-height,
        efficiency-rating: u85 ;; Default 85% efficiency
      }
    )

    (ok generator-id)
  )
)

;; Update generator status
(define-public (update-generator-status (generator-id uint) (new-status (string-ascii 20)))
  (let ((generator (unwrap! (map-get? water-generators { generator-id: generator-id }) ERR_INVALID_GENERATOR)))
    (asserts! (is-eq (get owner generator) tx-sender) ERR_UNAUTHORIZED)

    (map-set water-generators
      { generator-id: generator-id }
      (merge generator { status: new-status })
    )
    (ok true)
  )
)

;; ===========================================
;; ATMOSPHERIC MONITORING
;; ===========================================

;; Record atmospheric conditions
(define-public (record-atmospheric-conditions
    (generator-id uint)
    (humidity uint)
    (temperature int)
    (pressure uint)
    (wind-speed uint)
    (solar-irradiance uint))
  (let ((generator (unwrap! (map-get? water-generators { generator-id: generator-id }) ERR_INVALID_GENERATOR)))
    ;; Verify generator is active
    (asserts! (is-eq (get status generator) "active") ERR_GENERATOR_OFFLINE)

    ;; Record conditions
    (map-set atmospheric-conditions
      { generator-id: generator-id, timestamp: stacks-block-height }
      {
        humidity-percent: humidity,
        temperature-celsius: temperature,
        pressure-hpa: pressure,
        wind-speed-kmh: wind-speed,
        solar-irradiance: solar-irradiance,
        recorded-by: tx-sender
      }
    )

    ;; Update energy system based on conditions
    (update-energy-production generator-id solar-irradiance wind-speed)
  )
)

;; Calculate optimal production conditions
(define-read-only (calculate-production-potential (generator-id uint))
  (let (
    (conditions (map-get? atmospheric-conditions { generator-id: generator-id, timestamp: stacks-block-height }))
    (generator (map-get? water-generators { generator-id: generator-id }))
    (energy-system (map-get? energy-systems { generator-id: generator-id }))
  )
    (if (and (is-some conditions) (is-some generator) (is-some energy-system))
      (let (
        (humidity (get humidity-percent (unwrap-panic conditions)))
        (capacity (get capacity-liters-per-day (unwrap-panic generator)))
        (battery-level (get current-battery-level (unwrap-panic energy-system)))
        (efficiency (get efficiency-rating (unwrap-panic energy-system)))
      )
        (ok {
          production-potential: (/ (* capacity humidity efficiency) u10000),
          energy-sufficient: (>= battery-level MIN_ENERGY_LEVEL),
          humidity-adequate: (>= humidity MIN_HUMIDITY),
          recommended-action: (if (and (>= humidity MIN_HUMIDITY) (>= battery-level MIN_ENERGY_LEVEL))
                                "start-production"
                                "wait-for-conditions")
        })
      )
      (err ERR_INVALID_GENERATOR)
    )
  )
)

;; ===========================================
;; WATER PRODUCTION & QUALITY MANAGEMENT
;; ===========================================

;; Record water production batch
(define-public (record-water-production
    (generator-id uint)
    (batch-id uint)
    (volume uint)
    (quality-score uint)
    (ph-level uint)
    (tds-ppm uint)
    (energy-consumed uint))
  (let ((generator (unwrap! (map-get? water-generators { generator-id: generator-id }) ERR_INVALID_GENERATOR)))
    (asserts! (is-eq (get owner generator) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (>= quality-score MIN_WATER_QUALITY) ERR_POOR_WATER_QUALITY)

    ;; Record production
    (map-set water-production
      { generator-id: generator-id, batch-id: batch-id }
      {
        volume-liters: volume,
        quality-score: quality-score,
        ph-level: ph-level,
        tds-ppm: tds-ppm,
        production-timestamp: stacks-block-height,
        energy-consumed-kwh: energy-consumed,
        distribution-status: "pending"
      }
    )

    ;; Update energy consumption
    (update-energy-consumption generator-id energy-consumed)
  )
)

;; Update water distribution status
(define-public (update-distribution-status
    (generator-id uint)
    (batch-id uint)
    (new-status (string-ascii 20)))
  (let ((production (unwrap! (map-get? water-production { generator-id: generator-id, batch-id: batch-id }) ERR_INVALID_GENERATOR)))
    (map-set water-production
      { generator-id: generator-id, batch-id: batch-id }
      (merge production { distribution-status: new-status })
    )
    (ok true)
  )
)

;; ===========================================
;; COMMUNITY MANAGEMENT
;; ===========================================

;; Register desert community
(define-public (register-community
    (community-id uint)
    (name (string-ascii 100))
    (latitude int)
    (longitude int)
    (population uint)
    (daily-need uint)
    (priority uint))
  (begin
    (asserts! (and (< (abs-int latitude) (to-int MAX_COORDINATE)) (< (abs-int longitude) (to-int MAX_COORDINATE))) ERR_INVALID_COORDINATES)
    (asserts! (and (>= priority u1) (<= priority u5)) ERR_COMMUNITY_NOT_FOUND)

    (map-set desert-communities
      { community-id: community-id }
      {
        name: name,
        location: { latitude: latitude, longitude: longitude },
        population: population,
        daily-water-need-liters: daily-need,
        registered-by: tx-sender,
        registration-block: stacks-block-height,
        priority-level: priority
      }
    )
    (ok community-id)
  )
)

;; Calculate water supply vs demand for community
(define-read-only (calculate-water-balance (community-id uint))
  (let ((community (map-get? desert-communities { community-id: community-id })))
    (if (is-some community)
      (let (
        (daily-need (get daily-water-need-liters (unwrap-panic community)))
        ;; This would need to aggregate all generators serving this community
        (estimated-supply u1000) ;; Simplified for this example
      )
        (ok {
          daily-need: daily-need,
          estimated-supply: estimated-supply,
          balance: (if (>= estimated-supply daily-need) "surplus" "deficit"),
          coverage-percentage: (/ (* estimated-supply u100) daily-need)
        })
      )
      (err ERR_COMMUNITY_NOT_FOUND)
    )
  )
)

;; ===========================================
;; ENERGY SYSTEM INTEGRATION
;; ===========================================

;; Update energy system configuration
(define-public (configure-energy-system
    (generator-id uint)
    (solar-kw uint)
    (wind-kw uint)
    (battery-kwh uint)
    (grid-connected bool))
  (let ((generator (unwrap! (map-get? water-generators { generator-id: generator-id }) ERR_INVALID_GENERATOR)))
    (asserts! (is-eq (get owner generator) tx-sender) ERR_UNAUTHORIZED)

    (map-set energy-systems
      { generator-id: generator-id }
      {
        solar-capacity-kw: solar-kw,
        wind-capacity-kw: wind-kw,
        battery-capacity-kwh: battery-kwh,
        current-battery-level: u100, ;; Reset to full on reconfig
        grid-connected: grid-connected,
        last-maintenance: stacks-block-height,
        efficiency-rating: u85
      }
    )
    (ok true)
  )
)

;; Private function to update energy production
(define-private (update-energy-production (generator-id uint) (solar-irradiance uint) (wind-speed uint))
  (let ((energy-system (unwrap! (map-get? energy-systems { generator-id: generator-id }) ERR_INVALID_GENERATOR)))
    (let (
      (solar-production (/ (* (get solar-capacity-kw energy-system) solar-irradiance) u1000))
      (wind-production (if (>= wind-speed u15) ;; 15 km/h minimum for wind production
                         (/ (* (get wind-capacity-kw energy-system) wind-speed) u50)
                         u0))
      (total-production (+ solar-production wind-production))
      (current-level (get current-battery-level energy-system))
      (new-level (min-uint u100 (+ current-level (/ total-production u10))))
    )
      (map-set energy-systems
        { generator-id: generator-id }
        (merge energy-system { current-battery-level: new-level })
      )
      (ok new-level)
    )
  )
)

;; Private function to update energy consumption
(define-private (update-energy-consumption (generator-id uint) (energy-consumed uint))
  (let ((energy-system (unwrap! (map-get? energy-systems { generator-id: generator-id }) ERR_INVALID_GENERATOR)))
    (let (
      (current-level (get current-battery-level energy-system))
      (consumption-percent (/ (* energy-consumed u100) (get battery-capacity-kwh energy-system)))
      (new-level (if (>= current-level consumption-percent)
                   (- current-level consumption-percent)
                   u0))
    )
      (map-set energy-systems
        { generator-id: generator-id }
        (merge energy-system { current-battery-level: new-level })
      )
      (ok new-level)
    )
  )
)

;; ===========================================
;; DISTRIBUTION & LOGISTICS
;; ===========================================

;; Record water distribution to community
(define-public (record-distribution
    (distribution-id uint)
    (community-id uint)
    (generator-id uint)
    (volume uint)
    (transport-method (string-ascii 50))
    (cost-per-liter uint))
  (begin
    ;; Verify community and generator exist
    (asserts! (is-some (map-get? desert-communities { community-id: community-id })) ERR_COMMUNITY_NOT_FOUND)
    (asserts! (is-some (map-get? water-generators { generator-id: generator-id })) ERR_INVALID_GENERATOR)

    (map-set water-distribution
      { distribution-id: distribution-id }
      {
        community-id: community-id,
        generator-id: generator-id,
        volume-distributed: volume,
        distribution-date: stacks-block-height,
        transport-method: transport-method,
        cost-per-liter: cost-per-liter,
        verified-by: tx-sender
      }
    )
    (ok distribution-id)
  )
)

;; ===========================================
;; MONITORING & ANALYTICS
;; ===========================================

;; Get comprehensive generator status
(define-read-only (get-generator-status (generator-id uint))
  (let (
    (generator (map-get? water-generators { generator-id: generator-id }))
    (energy-system (map-get? energy-systems { generator-id: generator-id }))
    (conditions (map-get? atmospheric-conditions { generator-id: generator-id, timestamp: stacks-block-height }))
  )
    (ok {
      generator: generator,
      energy-system: energy-system,
      current-conditions: conditions,
      operational-status: (if (and (is-some generator) (is-some energy-system))
                            (if (and
                                  (is-eq (get status (unwrap-panic generator)) "active")
                                  (>= (get current-battery-level (unwrap-panic energy-system)) MIN_ENERGY_LEVEL))
                              "operational"
                              "limited")
                            "offline")
    })
  )
)

;; Get community water security status
(define-read-only (get-community-security (community-id uint))
  (let ((community (map-get? desert-communities { community-id: community-id })))
    (if (is-some community)
      (ok {
        community-info: (unwrap-panic community),
        water-balance: (unwrap-panic (calculate-water-balance community-id)),
        security-level: (if (>= (get coverage-percentage (unwrap-panic (calculate-water-balance community-id))) u100)
                          "secure"
                          "at-risk")
      })
      (err ERR_COMMUNITY_NOT_FOUND)
    )
  )
)

;; ===========================================
;; CLIMATE ADAPTATION STRATEGIES
;; ===========================================

;; Calculate climate resilience score
(define-read-only (calculate-resilience-score (generator-id uint))
  (let (
    (generator (map-get? water-generators { generator-id: generator-id }))
    (energy-system (map-get? energy-systems { generator-id: generator-id }))
    (conditions (map-get? atmospheric-conditions { generator-id: generator-id, timestamp: stacks-block-height }))
  )
    (if (and (is-some generator) (is-some energy-system))
      (let (
        (energy-diversity (+
          (if (> (get solar-capacity-kw (unwrap-panic energy-system)) u0) u25 u0)
          (if (> (get wind-capacity-kw (unwrap-panic energy-system)) u0) u25 u0)
          (if (get grid-connected (unwrap-panic energy-system)) u25 u0)
          u25)) ;; Base battery storage
        (maintenance-score (if (< (- stacks-block-height (get last-maintenance (unwrap-panic energy-system))) u1000) u100 u50))
        (efficiency-score (get efficiency-rating (unwrap-panic energy-system)))
      )
        (ok {
          overall-resilience: (/ (+ energy-diversity maintenance-score efficiency-score) u3),
          energy-diversity: energy-diversity,
          maintenance-status: maintenance-score,
          efficiency-rating: efficiency-score,
          recommendation: (if (< (/ (+ energy-diversity maintenance-score efficiency-score) u3) u70)
                            "upgrade-recommended"
                            "resilient")
        })
      )
      (err ERR_INVALID_GENERATOR)
    )
  )
)
