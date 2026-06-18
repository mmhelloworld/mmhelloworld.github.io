# mmhelloworld.github.io

The source for [mmhelloworld.github.io](https://mmhelloworld.github.io) — *The Joy of Programming!*

Built with [Astro](https://astro.build) and deployed to GitHub Pages via GitHub Actions.
(Migrated from Octopress/Jekyll; all original post URLs are preserved.)

## Develop

```bash
npm install
npm run dev        # local dev server at http://localhost:4321
npm run build      # production build into dist/
npm run preview    # preview the production build
npm run astro check # type-check
```

## Structure

- `src/content/blog/` — posts in Markdown. Front matter: `title`, `date` (bare `YYYY-MM-DD`), `tags`.
- `src/pages/blog/[...slug].astro` — generates the canonical `/blog/YYYY/MM/DD/title/` URLs from
  each post's `date` + filename. A build-time assertion fails the build if any existing URL would
  change.
- `src/pages/atom.xml.js` — RSS feed at `/atom.xml`.
- `src/layouts/`, `src/components/`, `src/styles/global.css` — site chrome and styling.
- `public/` — static assets served at the site root (`/images`, `/downloads`, `favicon.png`).
- `.github/workflows/deploy.yml` — builds and publishes to GitHub Pages on push to `master`.

## Adding a post

Create `src/content/blog/my-post-title.md`:

```markdown
---
title: "My Post Title"
date: 2026-01-15
tags: [Idris, JVM]
---

Post body in Markdown…
```

The URL becomes `/blog/2026/01/15/my-post-title/`. After adding a post, add its URL to
`CANONICAL_PATHS` in `src/pages/blog/[...slug].astro` (the assertion enforces the full set).

## Comments

Comments use [giscus](https://giscus.app). Set the real `repoId`/`categoryId` in `src/consts.ts`
(comments are hidden until configured).

## Deployment

On push to `master`, GitHub Actions builds the site and deploys it to GitHub Pages.
Repo **Settings → Pages → Source** must be set to **GitHub Actions**.
