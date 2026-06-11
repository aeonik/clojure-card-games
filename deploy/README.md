# Karbosh Deployment

`clojure-card-games` is the canonical source for the Karbosh application:

- Clojure server code: `src/clojure_card_games/karbosh/`
- ClojureScript client code: `src/clojure_card_games/karbosh/client.cljs`
- public client assets: `karbosh/`
- Karbosh tests: `test/clojure_card_games/karbosh/`

`dc3systems-new` is the static website repo for `dc3systems.com`. It may contain
legacy Karbosh code under `src/dc3systems/karbosh/`, but that copy is not
canonical and must not be deployed as the production Karbosh source.

Production paths:

```text
backend source: dc3systems.com:/home/dave/apps/clojure-card-games
static client:  dc3systems.com:/var/www/dc3systems.com/public_html/karbosh/
service:        karbosh.service
backend bind:   127.0.0.1:8090
start command:  /usr/local/bin/clojure -M:karbosh-server
```

Apache should proxy only public Karbosh routes, especially `/karbosh/ws`,
`/karbosh/api/`, and `/karbosh/admin`. Do not proxy nREPL.

## Contributor Deploy Workflow

Use `bin/karbosh-deploy` for normal human deploys. The lower-level
`clojure -T:build ...` tasks still exist, but the wrapper adds the checks people
usually forget:

- prints branch, commit, host, and SSH port
- runs tests and rebuilds ClojureScript before real deploys
- fails if generated assets changed and were not committed
- fails if the local branch is not pushed to its upstream
- gives dry-run commands for both compatible and restart deploys
- makes restart deploys require the explicit `DROP_ROOMS` confirmation

First-time local setup:

```sh
cp deploy/local.env.example .deploy.local.env
$EDITOR .deploy.local.env
source .deploy.local.env
```

The checked-in example contains no secrets. Production admin credentials live on
the server in `~/.config/karbosh/karbosh.env` and are sourced there by the reload
task.

Normal contributor loop:

```sh
bin/karbosh-deploy check
bin/karbosh-deploy plan-compatible
git push
bin/karbosh-deploy deploy-compatible
```

Structural/classpath deploy loop:

```sh
bin/karbosh-deploy check
bin/karbosh-deploy plan-restart
git push
bin/karbosh-deploy deploy-restart DROP_ROOMS
```

Use the restart path when files are renamed or deleted, namespaces move between
`.clj` and `.cljc`, dependencies/classpath change, systemd/Apache/env changes,
or room state is intentionally incompatible. It delete-syncs canonical deploy
directories and restarts the JVM, so active websocket sessions are dropped.

Quick health check:

```sh
bin/karbosh-deploy smoke
```

## Static Site Deploy

Deploy the static website from `~/Projects/dc3systems-new`, not from this repo:

```sh
cd ~/Projects/dc3systems-new
./bin/deploy-static
./bin/deploy-static deploy
```

The default mode is a dry run. The static deploy intentionally excludes
`karbosh/` and `src/` so it cannot overwrite the canonical Karbosh deployment or
ship legacy source into the public web root.

## Compatible Deploy

Use this for normal Karbosh source, ClojureScript, CSS, and static client
changes. It does not restart `karbosh.service`, so active in-memory rooms are
preserved where possible.

```sh
bin/karbosh-deploy plan-compatible
bin/karbosh-deploy deploy-compatible
```

`deploy-compatible` does three things:

- rsyncs canonical source/build/static files to
  `dc3systems.com:/home/dave/apps/clojure-card-games/`
- rsyncs `karbosh/` static assets to
  `dc3systems.com:/var/www/dc3systems.com/public_html/karbosh/`
- triggers the authenticated no-restart reload endpoint, then smoke checks
  `https://dc3systems.com/karbosh/api/health`

The reload endpoint is:

```text
POST /karbosh/admin/reload
```

The deploy task calls it over SSH by sourcing `~/.config/karbosh/karbosh.env` on
the server, so the admin password does not need to be present locally.

## Restart Deploy

Restart only when the running JVM cannot safely apply the change in place:

- dependency or classpath changes
- systemd unit changes
- Apache proxy or CSP changes
- environment/JVM/port/bind changes
- intentionally incompatible room or game-state schema changes

Restarting drops active in-memory rooms and disconnects players. The command
requires an explicit confirmation string:

```sh
bin/karbosh-deploy plan-restart
bin/karbosh-deploy deploy-restart DROP_ROOMS
```

`deploy-restart` syncs the canonical tree, deleting stale files inside the
source/static deploy directories, restarts `karbosh.service`, and smoke checks
the health endpoint.

## Build Tasks

Available build tasks:

```sh
clojure -T:build clean
clojure -T:build test
clojure -T:build cljs
clojure -T:build package-static
clojure -T:build release
clojure -T:build deploy-compatible-dry-run
clojure -T:build deploy-compatible
clojure -T:build deploy-restart-dry-run
clojure -T:build deploy-restart :confirm '"DROP_ROOMS"'
clojure -T:build smoke
clojure -T:build storage-report
clojure -T:build prod-storage-report
clojure -T:build compact-archive
clojure -T:build compact-prod-archive :confirm '"COMPACT_ARCHIVE"'
```

Useful deploy environment overrides:

```text
KARBOSH_DEPLOY_HOST=dc3systems.com
KARBOSH_SSH_PORT=22122
KARBOSH_RSYNC_RSH='ssh -p 22122'
KARBOSH_APP_DIR=~/apps/clojure-card-games/
KARBOSH_STATIC_DIR=/var/www/dc3systems.com/public_html/karbosh/
KARBOSH_HEALTH_URL=https://dc3systems.com/karbosh/api/health
KARBOSH_RESTART_COMMAND='sudo systemctl restart karbosh.service'
```

## Production Environment

The systemd unit loads:

```text
~/.config/karbosh/karbosh.env
```

Recommended values:

```sh
KARBOSH_ADMIN_PASSWORD=replace-with-a-long-random-password
KARBOSH_ALLOWED_ORIGINS=https://dc3systems.com,https://www.dc3systems.com
KARBOSH_MAX_MESSAGE_BYTES=8192
KARBOSH_MAX_ROOMS=128
KARBOSH_MAX_ROOM_CONNECTIONS=24
KARBOSH_MAX_WEBSOCKET_CONNECTIONS=256
KARBOSH_IDLE_ROOM_MS=300000
KARBOSH_ROOM_DIR=data/karbosh-rooms
KARBOSH_AUDIT_ENABLED=false
KARBOSH_AUDIT_DIR=data/karbosh-audit
KARBOSH_NREPL_ENABLED=true
KARBOSH_NREPL_BIND=127.0.0.1
KARBOSH_NREPL_PORT=7888
```

The admin panel is available at `/karbosh/admin` and uses HTTP Basic Auth. The
default username is `admin`; override it with `KARBOSH_ADMIN_USER` if needed. If
`KARBOSH_ADMIN_PASSWORD` is unset, admin routes return disabled responses.

The admin dashboard itself is protected. Direct room/game history links are
shareable so players can review games without dashboard access:

- `/karbosh/admin/rooms/{ROOM}/snapshot`
- `/karbosh/admin/rooms/{ROOM}/snapshot.edn`
- `/karbosh/admin/history/{ROOM}/{SEED}/{STARTED_AT}/snapshot`
- `/karbosh/admin/history/{ROOM}/{SEED}/{STARTED_AT}/snapshot.edn`

Read-only EDN API links expose the same data as smaller resources:

- `/karbosh/api/rooms/{ROOM}/snapshot`
- `/karbosh/api/rooms/{ROOM}/games`
- `/karbosh/api/rooms/{ROOM}/games/{SEED}/{STARTED_AT}`
- `/karbosh/api/rooms/{ROOM}/games/{SEED}/{STARTED_AT}/hands/{HAND_INDEX}`
- `/karbosh/api/rooms/{ROOM}/games/{SEED}/{STARTED_AT}/hands/{HAND_INDEX}/tricks/{TRICK_INDEX}`

## Durable Room Storage

`KARBOSH_ROOM_DIR` is the canonical production store. Each room is persisted as
one compact EDN file under `data/karbosh-rooms`, with websocket connection
objects stripped and human seats marked disconnected on disk. A room can be
unloaded from memory and later resumed from the same durable file.

Durable room files preserve:

- room code and room options
- current game state
- current game seed
- completed games within the room
- completed hand/trick/bid history
- bot personas and player seats

Use the storage report tasks to check the active store:

```sh
clojure -T:build storage-report
clojure -T:build prod-storage-report
```

The report includes active room storage size, legacy audit size, malformed room
files, zero-hand room files, and the largest durable rooms.

`KARBOSH_AUDIT_DIR` is legacy migration/forensic storage. The server can still
read it as a fallback for old snapshots, but `KARBOSH_AUDIT_ENABLED` should stay
`false` unless intentionally collecting short-term forensic records. Normal room
publishes no longer append full room snapshots to the audit log.

To compact a legacy append-only audit directory into durable room files:

```sh
clojure -T:build compact-archive
clojure -T:build compact-prod-archive :confirm '"COMPACT_ARCHIVE"'
```

The compactor writes durable room files first, then moves the old audit
directory to a timestamped backup such as
`data/karbosh-audit.compacted-20260607T182653Z`. Keep the backup until
historical room links have been spot-checked, then remove it manually when no
longer needed.

## Production REPL

nREPL must remain localhost-only. The production unit enables it by default so
we have a recovery path after bad hot reloads or expensive admin queries:

```sh
KARBOSH_NREPL_ENABLED=true
KARBOSH_NREPL_BIND=127.0.0.1
KARBOSH_NREPL_PORT=7888
```

The startup guard refuses non-loopback binds such as `0.0.0.0`. If you need to
disable the REPL for a deployment, set `KARBOSH_NREPL_ENABLED=false` in
`~/.config/karbosh/karbosh.env`.

Reach production nREPL only through an SSH tunnel:

```sh
ssh -L 7888:127.0.0.1:7888 dc3systems.com
```

Then connect CIDER or another nREPL client to `localhost:7888`.

Validation on the server:

```sh
ss -ltnp | grep 7888
```

Expected:

```text
127.0.0.1:7888
```

Bad:

```text
0.0.0.0:7888
```

From local without a tunnel, this should fail:

```sh
nc -vz dc3systems.com 7888
```

With the tunnel open, this should work:

```sh
nc -vz 127.0.0.1 7888
```

Do not expose nREPL through Apache, HTTPS proxying, or a public bind address.
Treat localhost-only nREPL as equivalent to SSH shell access.

## Hot Reload Shape

The HTTP server starts once with `clojure-card-games.karbosh.runtime/current-handler`.
Reloads install the current Karbosh handler and websocket callbacks into
`defonce` atoms. Existing rooms, websocket registries, bot timers, and metrics
remain in `defonce` state.

WebSocket callbacks dispatch through runtime handler atoms, so live sockets can
pick up newer message and close handlers after a reload where possible. Some
socket behavior can still be captured by old closures; use restart deploy for
state-breaking changes.

## Future Release Layout

The current production path is still:

```text
/home/dave/apps/clojure-card-games
```

The preferred long-term shape is a source-release tree with a `current` symlink:

```text
/home/dave/apps/karbosh/
  releases/
    2026-06-04T153000Z-a1b2c3d/
      deps.edn
      src/
      karbosh/
      build/
      VERSION
  current -> releases/2026-06-04T153000Z-a1b2c3d
```

This gives cleaner rollback while preserving source-based REPL and hot-load
workflow. Do not publish this app to Clojars just as a deployment mechanism, and
do not force an uberjar deployment yet.

## Systemd Hardening

The service includes modest hardening:

- `NoNewPrivileges=true`
- `UMask=0077`
- `PrivateTmp=true`
- `PrivateDevices=true`
- `ProtectSystem=full`
- `ProtectClock=true`, `ProtectHostname=true`, and `ProtectKernelLogs=true`
- kernel/control-group restrictions
- `RestrictAddressFamilies=AF_INET AF_INET6 AF_UNIX`
- `CapabilityBoundingSet=`
- `LimitNOFILE`, `TasksMax`, and `MemoryMax`

The production unit gives the process a 1536 MB cgroup ceiling and starts the
JVM with `-Xmx1280m`, leaving room for metaspace, threads, and native overhead.

Avoid `ProtectHome=true` unless the Clojure and Maven caches are moved elsewhere
or explicitly allowed.
