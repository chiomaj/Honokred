# Honokred

## Overview

**Honokred** is a **decentralized reputation system** built in **Clarity** that enables users to build, manage, and transfer reputation across different domains and applications while maintaining control and privacy.

Each “reputation domain” can define its own weighting system for **endorsements**, **engagements**, and **verifications**, allowing flexible scoring mechanisms tailored to various communities or use cases.

---

## Core Features

### 1. Domain Creation

Admins can create unique reputation domains with custom configurations for:

* Support, engagement, and validation weights (must sum ≤ 100)
* Minimum endorsements required
* Decay rate settings for reputation inactivity

**Function:**

```clarity
(create-reputation-domain title info support-weight engagement-weight validation-weight min-supports-required)
```

---

### 2. Endorsement System

Users can endorse others within a domain to build trust and credibility.
Each endorsement includes a strength level (1–10), optional note, and tags.
Endorsements can be revoked, affecting the recipient’s score.

**Functions:**

```clarity
(endorse-user domain-ref recipient strength note tags)
(remove-endorsement domain-ref recipient)
```

---

### 3. Engagement Recording

Users can record activities (e.g., contributions or achievements).
Each activity can later be verified by a domain admin or delegated verifier.

**Functions:**

```clarity
(record-activity domain-ref engagement-type points content-hash)
(verify-activity domain-ref engagement-id)
```

---

### 4. Verification System

Verifiers (either domain admins or authorized delegates) can confirm user credentials or achievements.
Each verification includes a **proof hash**, **tier (1–5)**, and optional expiry date.

**Functions:**

```clarity
(add-verification domain-ref account validation-type proof-hash tier expires-at)
(revoke-verification domain-ref account validation-type)
```

---

### 5. Delegation and Governance

Domain owners can:

* Assign **delegated verifiers** to handle validation operations.
* Allow **reputation management agents** for specific accounts.

**Functions:**

```clarity
(add-verification-provider domain-ref delegate title validation-types)
(revoke-verification-provider domain-ref delegate)
(delegate-reputation domain-ref agent expiry)
(remove-delegation domain-ref)
```

---

### 6. Privacy Control

Each user can define visibility rules for their reputation data.
They can choose what is public and who is allowed to view private data.

**Function:**

```clarity
(update-privacy-settings domain-ref public-score public-supports public-engagements public-validations approved-viewers)
```

---

### 7. Dynamic Reputation Calculation

Reputation scores are automatically recalculated after every update, factoring in:

* Endorsement count and strength
* Engagement frequency and verification
* Validation tier
* Time-based decay for inactivity

**Core Calculation Logic:**

* **Support score:** Scales based on minimum endorsement threshold.
* **Engagement score:** Proportional to activity count (up to 1000).
* **Validation score:** 5-tier scale mapped to 0–1000.
* **Final score:** Weighted sum with optional decay adjustment.

---

## Data Structures

### Maps

* `rep-domains`: Stores domain configurations.
* `user-rep`: Tracks user reputation data per domain.
* `user-endorsements`: Manages endorsement details.
* `user-verifications`: Stores validation proofs and levels.
* `rep-activities`: Records engagements and verification status.
* `verification-delegates`: Handles verifier roles.
* `rep-delegations`: Manages delegated reputation control.
* `user-privacy`: Stores privacy and access preferences.

---

## Security and Access Control

* Only domain owners can manage providers and governance.
* Only verifiers or delegates can issue or revoke verifications.
* Users can control their privacy and delegate management rights.
* All sensitive operations validate domain IDs and user permissions.

---

## Scoring and Decay

* Each user’s score decays gradually based on inactivity (default 1% per 1000 blocks).
* Decay applies during recalculation only.
* Weighted scoring ensures flexible tuning for each domain’s ecosystem.

---

## Summary

**Honokred** empowers communities, DAOs, and decentralized networks to **quantify and manage trust** through transparent, modular, and privacy-respecting reputation mechanics. It combines endorsements, verified actions, and adaptive scoring for multi-domain credibility that remains portable and verifiable on-chain.
