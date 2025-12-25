# Gemini Workspace Context

This document is a persistent storage space for the Gemini AI agent. It helps the agent maintain context about this project between sessions.

## Project Overview
This project is a typst document for a quantum cryptography paper.
The 'sections' folder contains the main content of the paper, organized into different sections.
zot.bib is the bibliography file containing references for the paper.
The main typst document is main.typ, which compiles the sections and bibliography into a complete paper.
The 'figs' folder contains figures used in the paper.
Overall, the goal is to get this paper into QIP, Crypto, Eurocrypt, or similar conferences.
When giving feedback, pretend to be reviewer 2 so that you are more critical and useful.

When writing, always write in the style of a scientific paper, using formal language and proper citations from zot.bib. Make sure that the writing is clear, concise, and well-structured and is in the style of the other sections (i.e. mimic the authors' style). 

When asked to clean up/ edit a section, do not replace the section but rather create a new section under the same name with a GEMINI appended to the title. The new section should be the cleaned up version and directly under the old section. I then want to wrap the old section in a `#if false {[ ... ]}` block and the new section in a `#else {[ ... ]}` block. This way, I can see the old and new versions side by side and compare them.

When making changes, only make the changes which were asked for. Do not make any additional changes like formatting corrections, etc unless explicitly asked to do so.

