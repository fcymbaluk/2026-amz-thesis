# 2026-10-20 — Chapter 2 evidence: highlights in Zotero, codes in ATLAS.ti

**Context.** The Chapter 2 guidance of 2026-10-18 placed the whole
evidence layer in Zotero: highlights as quotations, highlight colors as
the three dimensions, tags as codes, and a Markdown export of notes into
the repo. The codebook written the same day already doubted the export
path (Zotero's note-from-annotations drops colors and tags), and left the
coding tool open: the author's ATLAS.ti license had been renewed, and
ATLAS.ti 26 ships a local MCP server that Claude Code can call. The
question was whether to code in Zotero, with a read-only export through
its local API, or in a QDA tool with code hierarchy, groups, networks and
retrieval, at the price of a second tool in the loop.

**Decision.** Reading and highlighting stay in Zotero; coding moves to
ATLAS.ti, project `2026-amz-ch2`.

1. In Zotero, a highlight is a quotation, its color is the dimension
   (red d1, blue d2, green d3, yellow context), and its comment is the
   one-line claim. Tags are not used for coding any more.
2. Each corpus item's PDF is imported into ATLAS.ti straight from
   Zotero's storage folder, as a document named by the citation key (one
   per PDF attachment when a work is split into several files), and
   every highlight is placed on the PDF as a quotation, located by text
   match and pre-coded with the dimension code its color stands for
   (`d0-context`, `d1-conflict`, `d2-institutions`, `d3-governance`).
   The Zotero comment and a provenance line (annotation key, page,
   color) become the quotation comment. The importer is
   `scratch/zotero_to_atlasti.py` (Python, gitignored, outside the
   analysis pipeline): it reads the Zotero local API and Better BibTeX,
   writes nothing into Zotero, and talks to ATLAS.ti through the MCP
   server.
3. All other codes (period, voice, asset, field, level, direction,
   activity, actor, policy) are applied in ATLAS.ti. Coding is the
   author's act. Claude Code codes in ATLAS.ti only when asked, and marks
   what it applies (code group `ai-proposed`, a dated note in the
   quotation comment) until the author accepts or rejects it.
4. The ATLAS.ti project is the source of truth for codings and sits
   outside git; Zotero remains the source of truth for highlights and
   comments. The repo holds dated, regenerable exports of the quotation
   table in `ch2-historical/evidence/`, from which the evidence matrix is
   built.

Rejected: coding in Zotero with the local-API export (flat tags only,
no retrieval tools, and tag discipline enforced by script alone);
highlighting in ATLAS.ti instead of Zotero (drops Zotero, where the
reading and the bibliography live, from the loop); exporting Zotero
notes and importing them as plain documents (loses color and tags, and
every quotation would be re-marked by hand); moving annotated PDFs by
hand (Zotero keeps highlights in its database, not in the file). Also
superseded the same day: the importer's first version, which built one
plain-text document per source out of the highlights alone. It worked
(368 quotations placed exactly), but it stripped the text around each
quotation, which the author needs when reading and the agent needs when
asked to propose codes; Zotero's own full-text cache stops at 100 pages,
so it could not supply the context either.

**Rationale.** ATLAS.ti gives the codebook what Zotero tags cannot:
colored codes, code groups and folders, co-occurrence and query tools for
the retrieval of the post-coding constructs (phases, two arenas,
mechanisms A and B), memos linked to quotations, and networks. The MCP
server makes the coding layer readable and, on request, writable from
Claude Code, so the derived layer (matrix, gap reports, drafts with
citations) can be built from live codings rather than from a lossy
export. Keeping highlights in Zotero preserves the reading workflow, the
pinned citation keys, EPUB support and account sync as backup. The
PDF path was verified on 2026-10-20 with London and Kelly's *The Last
Forest*: the PDF imported from Zotero storage through the MCP server,
all 40 highlights were located in the text ATLAS.ti extracts (30 exact,
10 after whitespace and hyphenation normalization), and a quotation
placed by offset came back as the highlighted words with its code.

**Consequences.**

- `ch2-historical/GUIDANCE.md`: checklist items 1, 3 and 4 and the
  evidence-rules section rewritten (now "Zotero and ATLAS.ti workflow");
  `CLAUDE.md`, "Languages and tools", updated in the same commit.
- `ch2-historical/coding/codebook.md`: the dimension family `d0`–`d3`
  returns as codes (set by the importer from the color, single-valued);
  rule 2 keeps codes flat but allows ATLAS.ti folders and groups as
  organization; the per-highlight procedure splits into a Zotero step
  and an ATLAS.ti step; the "Export mechanics" section becomes "Import
  and export mechanics".
- A source is imported once its highlighting is complete: a re-import
  (`--replace`) deletes the document and every coding on it, so later
  highlights on an already coded source go into a dated addendum
  document (the same PDF imported again, with only the new highlights
  placed) rather than a rebuild.
- Highlights the text match cannot locate (poor OCR, text in figures)
  are listed by the importer for placement by hand.
- Highlights made before the codebook in colors outside the map
  (orange, gray, purple, magenta, black: about a quarter of the library's
  1,079 annotations) arrive without a dimension code; the author assigns
  the dimension in ATLAS.ti or recolors in Zotero before the import.
- Backup: the ATLAS.ti project is local only; a project bundle is
  exported to the Drive mirror at each milestone (or cloud sync is turned
  on), the author's act. MCP writes are unsaved until the author saves in
  the app: on 2026-10-20 an app restart after a document deletion
  reverted the project to its last save, dropping the four dimension
  codes created through the server. The importer recreates them.
- `ch2-historical/evidence/notes/` (Zotero note exports) is no longer
  planned; the evidence folder holds ATLAS.ti quotation-table exports.
