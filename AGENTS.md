# Agent Notes

## Branch And Commit Preferences

- Keep work on the current branch unless the user explicitly asks for a new branch.
- Create a new branch for experiments, risky UI/layout work, or broad AI/policy tuning when requested.
- Commit every working atomic change. Do not leave coherent completed work uncommitted.
- Keep commits scoped: source changes, generated ClojureScript assets, docs, and tests should be committed together only when they are one logical change.
- Do not revert user or other-agent changes unless the user explicitly asks.

## Push Preferences

- When the user asks to push or deploy, push the active branch to `origin` first.
- Report the branch name and latest commit SHA after pushing.
- If there are uncommitted changes, either commit them as an atomic change first or explain why they are not being pushed.

## Karbosh Deploy Preferences

- This repo is the canonical Karbosh source. Do not deploy legacy Karbosh code from `~/Projects/dc3systems-new`.
- Prefer the contributor deploy wrapper for normal source, CSS, ClojureScript, static client, and AI/policy changes:

```sh
bin/karbosh-deploy deploy-compatible
```

- `deploy-compatible` syncs source/static files, triggers the authenticated reload endpoint, and smoke-checks `https://dc3systems.com/karbosh/api/health`.
- Run `bin/karbosh-deploy plan-compatible` before a first deploy from an unfamiliar environment.
- Do not restart `karbosh.service` for ordinary changes. Restart only for dependency/classpath, environment, systemd, Apache/proxy/CSP, or incompatible room/game-state schema changes.
- Use `bin/karbosh-deploy plan-restart` and `bin/karbosh-deploy deploy-restart DROP_ROOMS` for structural/classpath deploys that need stale file cleanup and a fresh JVM.
- Restart deploy drops active runtime state and requires the explicit confirmation documented in `deploy/README.md`.
- After deploying, report the reload/smoke output, especially `:ok`, `:rooms`, and `:open-websockets` when present.

## Verification Preferences

- Run `clojure -M:test` before committing Clojure/ClojureScript logic changes.
- Run `clojure -M:karbosh-cljs` when `src/clojure_card_games/karbosh/client.cljs` changes, and include the rebuilt `karbosh/assets/js/main.js` when it changes.
- CSS-only changes do not require a ClojureScript rebuild.
