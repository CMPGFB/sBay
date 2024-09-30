;; sBay Marketplace
;; Author: Christopher Perceptions (Improved by Claude)
;; Powered by NoCodeClarity v2

;; Constants and Data Variables
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATUS (err u102))
(define-constant ERR_EXPIRED (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))

(define-data-var platform-owner principal tx-sender)
(define-data-var next-listing-id uint u1)
(define-data-var platform-balance uint u0)
(define-data-var platform-fee uint u100) ;; 10% fee, represented as basis points

;; Listing data structure
(define-map listings
  { id: uint }
  {
    seller: principal,
    price: uint,
    token: principal,
    status: (string-ascii 20),
    expiry: uint,
    buyer: (optional principal)
  })

;; Function to create a new listing
(define-public (list-item (price uint) (token principal) (expiry uint))
  (let
    ((listing-id (var-get next-listing-id)))
    (map-set listings
      { id: listing-id }
      {
        seller: tx-sender,
        price: price,
        token: token,
        status: "active",
        expiry: expiry,
        buyer: none
      })
    (var-set next-listing-id (+ listing-id u1))
    (ok listing-id)))

;; Function to buy an item
(define-public (buy-item (listing-id uint))
  (let
    ((listing (unwrap! (map-get? listings { id: listing-id }) ERR_NOT_FOUND)))
    (asserts! (is-eq (get status listing) "active") ERR_INVALID_STATUS)
    (asserts! (or (is-eq (get expiry listing) u0) (< block-height (get expiry listing))) ERR_EXPIRED)
    (let
      ((price (get price listing))
       (seller (get seller listing))
       (fee-amount (/ (* price (var-get platform-fee)) u1000)))
      (try! (stx-transfer? price tx-sender seller))
      (try! (stx-transfer? fee-amount seller (var-get platform-owner)))
      (map-set listings
        { id: listing-id }
        (merge listing {
          status: "sold",
          buyer: (some tx-sender)
        }))
      (var-set platform-balance (+ (var-get platform-balance) fee-amount))
      (ok true))))

;; Function to cancel a listing
(define-public (cancel-listing (listing-id uint))
  (let
    ((listing (unwrap! (map-get? listings { id: listing-id }) ERR_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get seller listing)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status listing) "active") ERR_INVALID_STATUS)
    (map-set listings
      { id: listing-id }
      (merge listing { status: "canceled" }))
    (ok true)))

;; Function to withdraw platform fees
(define-public (withdraw-fees)
  (let
    ((balance (var-get platform-balance)))
    (asserts! (is-eq tx-sender (var-get platform-owner)) ERR_UNAUTHORIZED)
    (try! (as-contract (stx-transfer? balance tx-sender (var-get platform-owner))))
    (var-set platform-balance u0)
    (ok balance)))

;; Function to update platform fee
(define-public (update-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender (var-get platform-owner)) ERR_UNAUTHORIZED)
    (asserts! (<= new-fee u1000) (err u105)) ;; Max 100% fee
    (var-set platform-fee new-fee)
    (ok true)))

;; Read-only functions
(define-read-only (get-listing (listing-id uint))
  (map-get? listings { id: listing-id }))

(define-read-only (get-platform-balance)
  (ok (var-get platform-balance)))

(define-read-only (get-platform-fee)
  (ok (var-get platform-fee)))

;; SIP-009 NFT Interface (optional, for token transfers)
(define-trait nft-trait
  (
    (transfer (uint principal principal) (response bool uint))
    (get-owner (uint) (response principal uint))
  ))
  