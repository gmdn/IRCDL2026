# Annotaterm @ IRCDL2026

> *Annotaterm: A Fragment-Based Approach for Annotating Discontinuous Named Entities and Terms*  
> Accepted at **22nd International Conference on Information and Research Science Connecting to Digital and Library Science (IRCDL 2026)**


**Annotaterm** is a lightweight web-based prototype for the manual annotation of **discontinuous named entities and domain-specific terms**.  
The tool accompanies the paper:

### Motivation
Most existing annotation tools assume that entities and terms correspond to **contiguous text spans**.  
However, in many real-world domains (administrative, legal, technical, biomedical), entities and terms are expressed through **non-contiguous fragments** (e.g., coordination or ellipsis).  
Annotaterm addresses this gap by introducing a **fragment-based annotation model**, where each fragment is treated as a first-class annotation unit and linked under a shared label.

### Key Features
- Manual annotation of **contiguous and discontinuous** entities and terms  
- Fragment-based representation with explicit character offsets  
- Simple web interface built with **R Shiny**  
- Tabular, analysis-friendly internal data model  
- Designed for terminology extraction and NER corpus creation  

### Scope and Status
This repository provides a **research prototype** intended to demonstrate feasibility and interaction design rather than a production-ready annotation platform.  
Several advanced features (collaborative annotation, export to BioC/BRAT, automated suggestions, deletion and editing of annotations) are currently under development.

### Code
The prototype is implemented in few lines of R code and can be run locally using R and the `shiny` package.

### Citation
If you use Annotaterm or refer to this work, please cite the accompanying paper:

```bibtex
@inproceedings{annotaterm2026,
  title     = {Annotaterm: A Fragment-Based Approach for Annotating Discontinuous Named Entities and Terms},
  author    = {Di Nunzio, Giorgio Maria and Vezzani, Federica},
  booktitle = {Proceedings of the 22nd Conference on Information and Research Science Connecting to Digital and Library Science (IRCDL 2026)},
  editor    = {Baraldi, Lorenzo and Cornia, Marcella and Carrara, Fabio and Cuculo, Vittorio and Daquino, Marilena and Marchesin, Stefano and Paolanti, Marina and Sarto, Sara},
  address   = {Modena, Italy},
  month     = feb,
  year      = {2026},
  note      = {19--20 February 2026},
  url       = {https://ircdl2026.unimore.it/}
}

