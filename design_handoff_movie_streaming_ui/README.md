# Handoff: Stamno Media — Movie/TV Streaming UI

## Overview
A simple movie & TV streaming platform UI (Jellyfin/Plex-style) for "Stamno Media": browse, library, movie/show detail, video player, and a Jellyseerr-style request/discovery flow.

## About the Design Files
The bundled file (`Stamno Media.dc.html`) is a **design reference built in HTML** — a working prototype showing intended layout, states, and interactions, not production code to copy directly. The task is to recreate this design in the target codebase's existing environment (React, Vue, native, etc.) using its established patterns/libraries, or to choose the most appropriate framework if none exists yet.

The file is self-contained and can be opened directly in a browser to click through every screen/state before implementing.

## Fidelity
**High-fidelity.** Colors, typography, spacing, and interactions shown are final — recreate pixel-perfectly with the target codebase's own component/styling system.

## Visual system (Stamno Design System)
Monochrome hacker-terminal aesthetic:
- **Type**: `"Courier New", Courier, monospace` everywhere (no other typeface). Uppercase labels, `letter-spacing: 0.1em`–`0.2em`.
- **Color**: near-black background (`#000`/`#0a0b0d`), white/gray text ramp (`#ffffff` primary, `#c0c0c0` secondary, `#808080` muted), 1px gray borders (`#666666` default, `#ffffff` strong emphasis). No color accents/neon used in this build — kept strictly monochrome per user direction (an earlier glitch/neon iteration was explicitly rejected).
- **Shape**: zero border-radius anywhere — all corners square.
- **Cards/borders**: 1px solid border, flat black/dark-gray fill, no shadows.
- **Background overlays**: two fixed full-viewport layers run at all times — a CRT scanline gradient and a subtle VHS tracking-noise texture (both `pointer-events:none`, low opacity).
- **Spacing scale**: 4/8/12/16/24/32/48/64px (`--space-1`…`--space-8`).

## Screens / Views

### 1. Home
- Sticky top nav (64px tall, 1px bottom border): logo "STAMNO_MEDIA" (bold, wide letter-spacing) far left, then nav links **Home / Movies / Shows / Library / Requests** (active link = white text + white 1px underline, inactive = gray). Right side: "SEARCH$" label + small square avatar box.
- Hero banner (440px tall): radial dark-gray gradient background, bottom-aligned content block (max-width 640px): title (large bold uppercase), meta row (year · duration · genre chip), synopsis line, and two buttons — "▶ Play" (primary/filled) and "More Info" (ghost/outlined).
- Three horizontal poster rows below the hero: **Continue Watching**, **Trending Now**, **Recently Added**. Each row has a label + two small square prev/next arrow buttons (28×28px, 1px border) that scroll the row by ~520px with smooth scrolling. Scrollbars are hidden (`scrollbar-width:none` + `::-webkit-scrollbar{display:none}`) since the arrows are the only scroll affordance.
- Poster cards: 160×240px, 1px border, dark-gray fill, big pale monogram letter (first letter of title) centered as placeholder art. Title (and year, where shown) are **hidden by default and only fade in on hover** (`opacity:0` → `opacity:1` on an absolutely-positioned inset overlay, `transition: opacity .15s`). Continue Watching cards additionally show a persistent thin progress bar at the bottom (always visible, not hover-gated).

### 2. Movies / Shows (separate screens, same layout)
- Page title ("Movies" or "Shows"), genre filter chips row (**All / Action / Scifi / Drama / Thriller** — active chip has white border + white text, inactive has gray border + gray text), then a responsive grid (`repeat(auto-fill, minmax(160px,1fr))`, 24px gap) of the same poster cards (hover-reveal title/meta as above). Movies show release year on hover; Shows show season count (e.g. "3 Seasons") instead of runtime.

### 3. Library
- Same layout/pattern as Movies/Shows but shows the full combined catalog (movies + shows), filterable by the same genre chip row.

### 4. Detail
- "← Back" link top-left.
- Backdrop band (340px, dark radial gradient) with a poster box (200×300px, monogram letter, white 1px border) overlapping it via `margin-top:-120px` (backdrop is `z-index:0`, poster/info block `z-index:1` so poster stacks correctly on top).
- Info block: title, meta row (year · duration · rating), one genre tag (square outlined tag component), synopsis paragraph, cast line, "▶ Play" + "+ Add to List" buttons.
- "More Like This" horizontal row of poster cards (same hover-reveal pattern), excludes the current title.

### 5. Player
- Top nav bar is **hidden** on this screen (`display:none` driven by screen state) for an immersive video view.
- Its own minimal bar: "← Back" + title.
- Centered video stage (16:9, dark radial gradient placeholder) with a static white triangular play glyph.
- Below: thin scrubber track (2px, gray) with a white fill representing progress, transport controls (prev/play-pause/next as text glyphs) + elapsed/total time, and right-aligned volume/captions/fullscreen glyphs.

### 6. Requests (Jellyseerr/Overseerr-style)
- Page title "Requests" + prominent primary "+ New Request" button (top-right) that jumps straight to the Discover tab.
- Two tabs, underline-active style like the top nav: **My Requests** / **Discover**.
- **My Requests**: dense list rows (not cards) — 48×48px monogram thumbnail, title + "{type} · {year} · Requested {date}" meta line, and a right-aligned status badge with three monochrome states distinguished by border style + text color only (no color-coding):
  - `AVAILABLE` — solid white (strong) border, white text
  - `PROCESSING` — solid gray (default) border, gray text
  - `PENDING` — **dashed** gray border, muted gray text
- **Discover**: poster grid of everything not yet requested. Each card shows title + a "+ Request" outlined button (white border) that inverts to filled white/black on hover; clicking moves that title into "My Requests" with status `PENDING` and date "JUST NOW".

## Interactions & Behavior
- All navigation (Home/Movies/Shows/Library/Requests, tabs, back links) is client-side state — no page reloads.
- Clicking any poster card/row item navigates to the Detail screen for that title.
- "▶ Play" (hero or Detail) navigates to the Player screen; Player's back link returns to Detail.
- Genre filter chips are shared filter state applied across Movies/Shows/Library.
- Row carousel arrows call `element.scrollBy({left, behavior:'smooth'})` on the row's scroll container.
- Hover reveal: poster title/meta text is invisible until the card (or, for Continue Watching/related cards, an inset overlay spanning the full card) is hovered — implemented as `opacity:0` → `opacity:1` with a `.15s` transition; underlying `:hover` bubbles correctly since the overlay spans the whole card.
- Requesting a title in Discover is optimistic/local: it immediately reclassifies the title into "My Requests" as `PENDING`.

## State Management
Minimal local UI state (no backend in this prototype):
- `screen`: `'home' | 'movies' | 'shows' | 'library' | 'detail' | 'player' | 'requests'`
- `activeId`: id of the title currently open in Detail/Player
- `filter`: active genre filter (`'ALL' | 'ACTION' | 'SCIFI' | 'DRAMA' | 'THRILLER'`), shared by Movies/Shows/Library
- `requestsTab`: `'mine' | 'discover'`
- `requestStatuses`: map of `titleId → 'AVAILABLE' | 'PROCESSING' | 'PENDING'`
- `requestDates`: map of `titleId → display date string`

For production, replace the local catalog array and request maps with real data: a media library API (metadata, artwork, playback URLs — analogous to a Jellyfin/Plex/Emby server) and a request-management API (analogous to Jellyseerr/Overseerr, which itself talks to Sonarr/Radarr) for the Requests screen's create/list/status behavior.

## Design Tokens
Colors:
- `--black:#000000` `--void:#0a0b0d` `--gray-1:#1a1a1a` `--gray-2:#333333` `--gray-3:#666666` `--gray-4:#808080` `--gray-5:#c0c0c0` `--gray-6:#e8e8e8` `--white:#ffffff`
- Semantic: `--bg-page:#000` `--bg-panel:#1a1a1a` `--fg-primary:#fff` `--fg-secondary:#c0c0c0` `--fg-muted:#808080` `--border-default:#666666` `--border-strong:#ffffff`

Typography:
- Font: `"Courier New", Courier, monospace` (only face used)
- Sizes: `--text-xs:0.75rem` `--text-sm:0.875rem` `--text-base:1rem` `--text-md:1.125rem` `--text-lg:1.5rem` `--text-xl:2rem` `--text-2xl:3rem`
- Tracking: `--tracking-wide:0.1em` `--tracking-wider:0.2em`
- Weights: `--weight-normal:400` `--weight-bold:700`

Spacing: `--space-1:4px` `--space-2:8px` `--space-3:12px` `--space-4:16px` `--space-5:24px` `--space-6:32px` `--space-7:48px` `--space-8:64px` `--space-9:96px`

Borders/Radius: `--border-width:1px`, radius is **always 0** (no rounded corners anywhere in this design).

## Assets
No photography/illustration — poster art is a placeholder (a large pale first-letter monogram on a dark-gray card). Real poster/backdrop images should replace these placeholders in production. No icon library is used; all affordances are text/glyph-based (Courier glyphs like ▶ ‹ › ◄◄ ❚❚ ►►), consistent with the source design system's "no icon font" rule.

## Files
- `Stamno Media.dc.html` — the full prototype (all 7 screens). It's a single-file component: an inline-styled HTML template plus a small JS class holding state/data. Treat the JS class's `renderVals()` method as the source of truth for what data/derived state each screen needs, and the template markup as the source of truth for DOM structure and inline styles.
