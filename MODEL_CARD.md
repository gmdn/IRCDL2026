> **Research use only. Not a medical device.**
> This software was developed in the HEREDITARY project as a research prototype. It has not been assessed or certified under the EU Medical Device Regulation (EU) 2017/745. It must not be used to diagnose, treat, monitor or make any other decision about individual patients.

## 1. Overview

| Field | Value |
|---|---|
| Name | Annotaterm — original IRCDL2026 conference prototype |
| Version | Conference version preceding the IJDL extension; formal release version/tag/commit not supplied |
| Type | R Shiny annotation software and built-in illustrative text; no trained model or user-study dataset |
| Owner partner and contact | University of Padua (UNIPD); Giorgio Maria Di Nunzio, giorgiomaria.dinunzio@unipd.it |
| License | Apache-2.0 |
| Repository and paper | https://github.com/gmdn/IRCDL2026 ; https://ceur-ws.org/Vol-4240/paper43.pdf |
| Release date | February 2026 |
| Related task or deliverable | HEREDITARY WP3; misuse assessment contributes to D9.1 |
| How to cite | Di Nunzio, G.M., Vezzani, F. (2026). *Annotaterm: A Fragment-Based Approach for Annotating Discontinuous Named Entities and Terms*. Proceedings of IRCDL 2026, CEUR Workshop Proceedings, Vol. 4240. https://ceur-ws.org/Vol-4240/paper43.pdf |

This is the original conference prototype that preceded the extended IJDL work. For the extended article, see [*A fragment-based model and annotation environment for discontinuous and overlapping terms*](https://doi.org/10.1007/s00799-026-00447-x); its software and documentation are in the [IJDL2026 repository](https://github.com/gmdn/IJDL2026). The IJDL user evaluation, questionnaire data and usability scores are not part of this conference release.

## 2. Intended use

**Primary intended use.** Manual annotation of contiguous and discontinuous named entities and terms for terminology research, linguistic-resource creation and digital-library curation.

**Intended users.** Researchers, developers, terminology specialists and annotators.

**Out-of-scope uses.** This release must not be used for:
- clinical diagnosis, prognosis or treatment of individual patients;
- decisions about individuals, including employment, insurance or access to care;
- re-identification, surveillance, unauthorized processing of personal data, or profiling;
- entering personal, patient or confidential data into an unassessed deployment;
- treating manual annotations as independently verified facts or validated clinical labels;
- claiming performance on populations or tasks that have not been evaluated.

These are research-scope and responsible-use statements. They do not amend the software license to be confirmed by the owner.

## 3. Data provenance

| Dataset or input | Source partner | Type | Population or corpus | Ethics approval or access conditions |
|---|---|---|---|---|
| Built-in demonstration text (`annotaterm/ui.R`) | UNIPD software authors | Constructed illustrative text | Generic sentences about waste management and disposal; no people or clinical records. Population demographics: N/A. | Distributed in source; software license to confirm |
| User-study or questionnaire data | Not applicable | Not applicable | None for this conference release, as confirmed by the release owner; no survey files in the uploaded archive | Not applicable |

No participant/patient records or training corpus are included. Ordinary public author attribution appears in the documentation. ATE-IT, GutBrainIE and DETECH are discussed in the paper as possible future evaluation settings, not as completed evaluations or bundled datasets.

**Input handling.** The configured Shiny host processes user-entered text and text selections. Annotation and composite-builder state are created inside the server function as per-session reactive values. CSV and JSON download handlers create files for download. No JSON import/file-upload interface, application database, telemetry, input logger or explicit third-party API call was found in this version. Server location, infrastructure logs, backups and temporary-file retention remain deployment-dependent; no deletion guarantee is claimed.

## 4. Method

- **Algorithm or architecture.** Manual R Shiny client–server workflow. Simple and composite annotations are represented by segment records with `term_id`, `type`, `segment`, `start` and `finish` fields. A shared term identifier groups segments; the application provides a table, highlighted preview and CSV/JSON export.
- **Training setting.** Not applicable. The fragment-based model is a data representation, not a trained machine-learning model. No training or model-inference code is included.
- **Privacy techniques.** A generic non-personal example and session-specific annotation state are provided. No differential privacy, secure aggregation or anonymization of arbitrary user inputs is implemented or claimed.

## 5. Performance

- **Evaluation data.** No human-participant study or questionnaire data for this conference release. The built-in example illustrates interaction only.
- **Metrics and results.** Quantitative usability, annotation accuracy, timing and inter-annotator agreement were deferred to future work in the conference paper. No IJDL usability scores are attributed to this version.
- **Results by subgroup.** Not applicable: no participant evaluation was performed for this release.

## 6. Known limitations and risks

- **Generalizability.** A feasibility prototype and illustrative text do not establish effectiveness across domains, languages, devices or user populations. No clinical validity is established.
- **Known biases and confounders.** Annotation choices reflect the annotator's interpretation. No systematic empirical bias or confounder analysis is available for this release.
- **Snapshot and paper differences.** The uploaded IRCDL repository snapshot includes delete-last, delete-by-ID and clear-all controls, exact duplicate-span checks and a minimum-two-segment composite check. Section 4.3 of the conference paper describes these capabilities as future work. This card records the inspected snapshot; it does not establish when each control was introduced.
- **Failure modes.** Source text remains editable after annotation; no text-locking or automatic offset-invalidation guard was found. Duplicate-span rules can prevent reuse of a segment in another annotation, including some intended overlap/shared-fragment cases. Review outputs against the source and keep input text stable.
- **Version-specific scope.** This version has CSV/JSON export but no JSON import or later IJDL fragment-identifier and label-entry workflow. Do not infer full feature parity with the IJDL implementation.
- **Privacy risks.** Users could enter identifying or confidential text. The application contains no authentication layer; any external access control and infrastructure retention must be verified before hosting.
- **Model privacy tests.** Membership-inference and model-inversion testing are not applicable because no trained model is included.
- **Review limits.** Retrospective source/example inspection and common credential-pattern checks on 3 November 2026 found no embedded credentials or private research records. This does not establish historical pre-release scan status or constitute a formal security audit.

## 7. Responsible use and reporting

- **Terms of use.** Confirm and document the software license for this repository. The conference paper's CC-BY-4.0 publication license and the later IJDL software license do not by themselves establish this release's software terms.
- **Deployment.** Use non-personal research text. Verify the Shiny host's location, access permissions, logs, download temporary files, backups and retention/deletion policy. Keep downloaded annotations in an appropriate location. These are recommended checks, not a claim that hosting safeguards have been implemented.
- **Expert review.** Check segments, offsets and source context before downstream use. Delete/clear controls remove annotation state; they do not certify deletion of host logs, backups or previously downloaded exports.
- **Contact for misuse or vulnerability reports.** giorgiomaria.dinunzio@unipd.it.

## 8. Acknowledgement

Partially funded by the European Union under grant agreement No 101137074 (HEREDITARY). Views and opinions expressed are however those of the author(s) only and do not necessarily reflect those of the European Union or the European Health and Digital Executive Agency (HaDEA). Neither the European Union nor the granting authority can be held responsible for them.
