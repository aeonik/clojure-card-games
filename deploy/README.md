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
clojure -T:build test
clojure -T:build cljs
clojure -T:build package-static
clojure -T:build deploy-compatible
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
clojure -T:build test
clojure -T:build cljs
clojure -T:build release
clojure -T:build deploy-restart :confirm '"DROP_ROOMS"'
```

`deploy-restart` syncs the canonical tree, restarts `karbosh.service`, and smoke
checks the health endpoint.

## Build Tasks

Available build tasks:

```sh
clojure -T:build clean
clojure -T:build test
clojure -T:build cljs
clojure -T:build package-static
clojure -T:build release
clojure -T:build deploy-compatible
clojure -T:build deploy-restart :confirm '"DROP_ROOMS"'
clojure -T:build smoke
```

Useful deploy environment overrides:

```text
KARBOSH_DEPLOY_HOST=dc3systems.com
KARBOSH_APP_DIR=~/apps/clojure-card-games/
KARBOSH_STATIC_DIR=/var/www/dc3systems.com/public_html/karbosh/
KARBOSH_HEALTH_URL=https://dc3systems.com/karbosh/api/health
KARBOSH_RESTART_COMMAND='systemctl --user restart karbosh.service'
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
KARBOSH_IDLE_ROOM_MS=14400000
KARBOSH_AUDIT_ENABLED=true
KARBOSH_AUDIT_DIR=data/karbosh-audit
KARBOSH_NREPL_ENABLED=false
KARBOSH_NREPL_BIND=127.0.0.1
KARBOSH_NREPL_PORT=7888
```

The admin panel is available at `/karbosh/admin` and uses HTTP Basic Auth. The
default username is `admin`; override it with `KARBOSH_ADMIN_USER` if needed. If
`KARBOSH_ADMIN_PASSWORD` is unset, admin routes return disabled responses.

Rendered room histories are available at `/karbosh/admin/rooms/{ROOM}/snapshot`
under the same Basic Auth. Raw room snapshots are available at
`/karbosh/admin/rooms/{ROOM}/snapshot.edn`. The server also appends sanitized EDN
room snapshots to `KARBOSH_AUDIT_DIR` through a core.async writer after room
publishes and before room deletion. The audit log preserves seeds, deals, hands,
bids, tricks, and completed hand histories for long-term bot/game analysis, but
omits live websocket connection objects.

## Production REPL

nREPL is optional and must remain localhost-only. Enable it only through
environment:

```sh
KARBOSH_NREPL_ENABLED=true
KARBOSH_NREPL_BIND=127.0.0.1
KARBOSH_NREPL_PORT=7888
```

The startup guard refuses non-loopback binds such as `0.0.0.0`.

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

Avoid `ProtectHome=true` unless the Clojure and Maven caches are moved elsewhere
or explicitly allowed.
