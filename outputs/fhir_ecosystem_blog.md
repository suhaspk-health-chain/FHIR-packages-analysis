# The Global FHIR Ecosystem: What 75,000 Resource Definitions Tell Us About the Future of Healthcare Data

*A data-driven look at how the international healthcare community is building the infrastructure for medical interoperability — and what beginners need to know to get started.*

---

## The Problem No One Talks About

You have seen this scene before. A patient walks into a new clinic. The receptionist hands them a clipboard loaded with forms: name, date of birth, insurance, medications, allergies, surgical history. The patient has filled out these same forms a dozen times at a dozen different providers. Somewhere across town, their old hospital has all this information sitting in a database — but the new clinic cannot read it.

This is not a data problem. The data exists. It is an *interoperability* problem — different systems storing the same information in incompatible formats, unable to exchange it automatically.

FHIR was built to fix this.

---

## What Is FHIR?

**FHIR** (Fast Healthcare Interoperability Resources, pronounced "fire") is an international standard published by HL7 International that defines a universal grammar for healthcare data exchange. Think of it as the HTTP of healthcare — just as web browsers and servers can communicate because they all speak HTTP, healthcare systems that implement FHIR can exchange patient data regardless of which vendor built them.

FHIR achieves this by defining **resource types** — standardized templates for every kind of healthcare object:

- A **Patient** resource holds demographics, identifiers, and contact information
- An **Observation** resource holds a lab result, a vital sign, or a survey response
- a **Medication** resource describes a drug with dose and route
- A **Coverage** resource represents an insurance plan
- A **Claim** resource represents a billing transaction

When every system agrees to represent a Patient the same way, data flows freely.

---

## The Dataset: 75,411 Resources Across 1,096 Packages

This analysis covers the complete public FHIR package registry — every Implementation Guide (IG) that has been formally published and registered with HL7. The dataset was scraped from the [FHIR Cross-Implementation Guide (XIG) index](https://packages2.fhir.org/xig) and cross-referenced with the [official HL7 IG Registry](https://github.com/FHIR/ig-registry).

| Metric | Value |
|--------|-------|
| Total resource definitions | 75,411 |
| Unique published packages | 1,096+ |
| Distinct resource types | 23 |
| FHIR versions covered | R2, R3, R4, R4B, R5, R6 |
| Countries/realms represented | 30+ |
| Contributing organizations | 400+ |

---

## Finding 1: FHIR R4 Has Won the Market

If you are starting a new healthcare integration project today, there is one version you should target: **FHIR R4**.

Of the 1,096 packages in the registry, **908 (83%) are R4 packages**. The next largest group is R5 with 110 packages (10%), followed by R3 with 47 (4%). R4B and R6 together account for the remaining 3%.

| FHIR Version | Packages | Share |
|-------------|----------|-------|
| R4 | 908 | 82.8% |
| R5 | 110 | 10.0% |
| R3 | 47 | 4.3% |
| R4B | 25 | 2.3% |
| R6 | 6 | 0.5% |

This is not surprising. FHIR R4 was published as the first "normative" (stable, backward-compatible) version in 2019. Every major US federal regulation mandating FHIR — the CMS Interoperability Rule, the ONC 21st Century Cures Act — specifies R4. The healthcare industry has had six years to converge on it.

**What this means for beginners:** Learn R4 first. R5 is worth understanding for future projects, but R4 is where 95% of production systems live today.

---

## Finding 2: Two Resource Types Dominate Everything

Of the 23 recognized resource types in the registry, two account for the vast majority of all definitions:

| Resource Type | Count | Share |
|--------------|-------|-------|
| ValueSet | 29,056 | 38.5% |
| StructureDefinition | 28,329 | 37.6% |
| CodeSystem | 7,659 | 10.2% |
| SearchParameter | 2,095 | 2.8% |
| StructureMap | 1,353 | 1.8% |
| ConceptMap | 1,254 | 1.7% |
| *All others* | 5,665 | 7.5% |

**ValueSets** and **StructureDefinitions** together represent 76% of everything in the FHIR ecosystem. This reflects a fundamental truth about healthcare interoperability: most of the hard work is not in transmitting data — it is in agreeing on *what the data means*.

### What Is a StructureDefinition?

A StructureDefinition is a machine-readable blueprint. It specifies:

- Every field a resource must or may contain
- The data type of each field (string, date, code, reference to another resource)
- Which fields are required (cardinality)
- Which terminology to use for coded fields

For example, the US Core Patient profile (a StructureDefinition) says: a US patient record *must* include a patient identifier, *must* include at least one name, *should* include race and ethnicity (because US reporting requires it), and gender must use a specific code list.

### What Is a ValueSet?

A ValueSet is a curated list of allowed values for a field. They prevent the chaos of free text. Without a ValueSet, a gender field might contain "Male", "male", "M", "1", "Man" — all meaning the same thing but impossible to process consistently. A ValueSet says: this field must contain exactly one of these codes, drawn from this specific code system.

### What Is a CodeSystem?

A CodeSystem is the dictionary that ValueSets draw from. SNOMED CT (clinical terms), LOINC (lab tests), ICD-10 (diagnoses), RxNorm (medications) — these are all CodeSystems. They give each clinical concept a unique, unambiguous code with a precise definition.

The 7,659 CodeSystem definitions in the registry represent custom code systems created for specific domains — many national health agencies publish their own CodeSystems for local concepts that do not exist in international standards.

---

## Finding 3: The United States Leads the World in FHIR Adoption

The US is the single largest contributor to the FHIR package ecosystem, with more officially registered Implementation Guides than any other country.

This is entirely by design. US federal regulation has driven adoption at a scale no other country has matched:

- **CMS Interoperability and Prior Authorization Final Rule (CMS-0057F):** Requires all Medicare and Medicaid payers to expose patient data via FHIR APIs, support provider-to-provider data exchange, and automate prior authorization workflows using FHIR-based standards
- **ONC 21st Century Cures Act Final Rule:** Prohibits "information blocking" and requires EHR vendors to expose patient records via FHIR APIs accessible to third-party apps
- **Da Vinci Project:** A coalition of payers, providers, and EHR vendors building FHIR-based solutions for value-based care — covering prior authorization, coverage determination, clinical data exchange, and more

The result: US healthcare organizations have produced IGs covering every domain from pharmacy benefit management to social determinants of health to genomics.

**International contributors** include Australia (AU), New Zealand (NZ), Canada (CA), Germany (DE), Netherlands (NL), Finland (FI), and the Nordic countries — all have national programs to implement FHIR for their healthcare systems. The "UV" (Universal) realm represents guides intended to work across all countries, maintained by HL7 International itself.

---

## Finding 4: FHIR Has Grown 62% in a Decade

FHIR has been through six major versions since its introduction:

| Version | Year | Official Resource Types | Key Development |
|---------|------|------------------------|-----------------|
| R2 (DSTU2) | 2015 | 103 | First published specification |
| R3 (STU3) | 2017 | 121 | Added 34 types, retired 17 |
| R4 | 2019 | 150 | First normative release |
| R4B | 2022 | 144 | Pharmaceutical-focused branch |
| R5 | 2023 | 167 | Current stable release |
| R6 | ~2026 | 160* | Experimental/in-progress |

*R6 resource count is preliminary*

### What Changed at Each Transition?

**R2 to R3** added 34 resource types including CapabilityStatement, CodeSystem, ConsentManagement, GraphDefinition, and StructureMap. It removed placeholder categories like "Administrative" and "Clinical" that existed in R2 as organizational groupings rather than real resources.

**R3 to R4** was the biggest leap — adding 40 new types including a complete pharmaceutical domain (MedicinalProduct, Ingredient, SubstanceSpecification) and new research types (Evidence, EvidenceVariable, ResearchDefinition). It removed 13 deprecated types that had been replaced.

**R4 to R4B** was a targeted update to the pharmaceutical/regulatory domain, replacing the sprawling MedicinalProduct family with cleaner types like MedicinalProductDefinition, Ingredient, AdministrableProductDefinition, and PackagedProductDefinition.

**R4B to R5** added 30 types including major new categories: Requirements (for formal requirements capture), ActorDefinition (for defining system actors), BiologicallyDerivedProductDispense, GenomicStudy, and InventoryItem. Seven types were retired.

---

## Finding 5: The Ecosystem Is Dominated by Conformance Resources, Not Clinical Data

A common misconception: most FHIR resources in production carry *clinical data* — patient vitals, diagnoses, lab results. The package registry tells a different story.

The top resource types — ValueSet, StructureDefinition, CodeSystem — are all **conformance resources**. They do not carry patient data. They define *rules* that patient data must follow. This makes sense: Implementation Guides are rulebooks, not records. The actual patient data flows through FHIR APIs at runtime; the package registry captures the *agreements* about how that data should be shaped.

This distinction matters for understanding what a FHIR developer actually does day-to-day:

- **Implementers** read StructureDefinitions to understand what fields to populate
- **Terminology specialists** maintain ValueSets and CodeSystems
- **Validators** check that real data conforms to the StructureDefinitions
- **Mapping engineers** write StructureMaps to convert legacy HL7 v2 or CDA documents to FHIR

---

## Getting Started with FHIR: A Beginner's Roadmap

Based on this dataset, here is where to focus your learning:

### Step 1: Understand the Core Resource Types
Start with the resources you will encounter most in production: Patient, Observation, Condition, Medication, MedicationRequest, AllergyIntolerance, DiagnosticReport, Encounter, and Procedure. These cover 80% of clinical use cases.

### Step 2: Learn R4 First
Target FHIR R4 for any project starting today. The [FHIR R4 specification](https://hl7.org/fhir/R4/) is at hl7.org/fhir/R4. The US Core R4 Implementation Guide ([hl7.org/fhir/us/core](https://hl7.org/fhir/us/core/)) is the baseline for US-based work.

### Step 3: Understand Profiles vs. Base Resources
Base resources are generic. Profiles (StructureDefinitions that constrain a base resource) are specific. US Core Patient is a profile of the base Patient resource. Learning to read profiles is the core skill of FHIR implementation.

### Step 4: Explore the Package Registry
Use the [FHIR Package Registry](https://packages.fhir.org) to browse published IGs. If you are working in a specific domain (oncology, prior authorization, pharmacy), there is almost certainly a Da Vinci, CodeX, or domain-specific IG already published.

### Step 5: Use Validation Tools
The [FHIR Validator](https://validator.fhir.org) checks whether a FHIR resource actually conforms to its profile. Running validation early and often prevents data quality problems downstream.

---

## What This Dashboard Covers

This interactive dashboard lets you explore the full FHIR package dataset across six lenses:

- **Overview:** The big-picture numbers with key insights and an interactive plot builder
- **Evolution:** How resource types have changed across every FHIR version transition
- **Global Landscape:** Which countries contribute the most Implementation Guides
- **Data Hierarchy:** How the 75K resources break down by FHIR version and type
- **Resource Catalog:** Every resource type with count and percentage share
- **US Deep Dive:** Detailed view of the largest national contributor
- **Data Tables:** Full browsable dataset for hands-on exploration
- **Verification:** Data provenance and methodology

---

## Conclusion

The FHIR package registry is a window into the ambition of the global healthcare interoperability movement. Over 1,000 published rulebooks. 75,000 resource definitions. Contributions from 30+ countries. And a clear winner: FHIR R4, embraced by the market so thoroughly that four out of five published packages target it.

For anyone entering the healthcare technology space, FHIR is no longer optional knowledge. It is the plumbing of modern healthcare data exchange — and understanding its structure, its vocabulary, and its ecosystem is the foundation everything else is built on.

---

*Data sourced from the HL7 FHIR XIG Registry and HL7 IG Registry. Analysis conducted using R. Dashboard built with R Shiny.*

*Published by Suhas P K | March 2026*
