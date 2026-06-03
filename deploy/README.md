# Karbosh Deployment

`clojure-card-games` is the canonical source for the Karbosh application:

- Clojure server code: `src/clojure_card_games/karbosh/`
- ClojureScript client code: `src/clojure_card_games/karbosh/client.cljs`
- public client assets: `karbosh/`
- Karbosh tests: `test/clojure_card_games/karbosh/`

`dc3systems-new` is the site/deploy shell. It owns the Apache/systemd deployment
wiring used by `dc3systems.com`, but it should not be the place where Karbosh app
code is edited. Production currently runs the backend from:

```text
/home/dave/apps/clojure-card-games
```

and serves public static files from:

```text
/var/www/dc3systems.com/public_html/karbosh
```

The multiplayer backend normally listens on `127.0.0.1:8090`. Apache needs to proxy
`/karbosh/ws` and `/karbosh/api/` to that process; see `apache-karbosh.conf`.
This step requires privileged access because the active virtual host lives under
`/etc/apache2/sites-available/`.

After syncing the app source to the server:

```sh
cd /home/dave/apps/clojure-card-games
clojure -M:karbosh-server
```

## Normal No-Restart Deploy

Do not restart `karbosh.service` for ordinary Karbosh source, CLJS, CSS, or HTML
changes. A restart drops all in-memory rooms and disconnects active games.

From this repo, after committing and building the client:

```sh
clojure -M:karbosh-cljs
clojure -M:test -d test/clojure_card_games/karbosh
rsync -avz deps.edn src karbosh build test dc3systems.com:~/apps/clojure-card-games/
rsync -avz karbosh/ dc3systems.com:/var/www/dc3systems.com/public_html/karbosh/
```

Then ask the running JVM to reload the server-side Karbosh namespaces:

```sh
ssh dc3systems.com 'set -a; . ~/.config/karbosh/karbosh.env; set +a; curl -fsS -u "${KARBOSH_ADMIN_USER:-admin}:$KARBOSH_ADMIN_PASSWORD" -X POST https://dc3systems.com/karbosh/admin/reload'
```

That reload endpoint is authenticated with the same HTTP Basic credentials as the
admin panel. It reloads only server-side Karbosh namespaces that are already on
the JVM classpath. The live rooms, websocket registry, bot timers, and metrics
are held in `defonce` atoms, so they survive the in-process reload.

Static client files are served by Apache from the public `karbosh/` directory.
Existing browsers may need a page refresh to pick up a new `main.js` or CSS, but
the games themselves do not need to be killed.

## Restart-Required Changes

Restart `karbosh.service` only when the running JVM cannot apply the change in
place. That includes:

- `deps.edn` dependency/classpath changes that introduce new libraries
- systemd unit changes
- Apache proxy changes
- environment changes in `~/.config/karbosh/karbosh.env`
- port/bind/JVM option changes
- intentionally incompatible room-state schema changes

If a restart is unavoidable, warn players first because active room state is
currently in memory.

For the admin panel, set a password before starting the service. The systemd unit
loads this optional file:

```sh
mkdir -p ~/.config/karbosh
chmod 700 ~/.config/karbosh
cat > ~/.config/karbosh/karbosh.env <<'EOF'
KARBOSH_ADMIN_PASSWORD=replace-with-a-long-random-password
KARBOSH_ALLOWED_ORIGINS=https://dc3systems.com,https://www.dc3systems.com
KARBOSH_MAX_MESSAGE_BYTES=8192
KARBOSH_MAX_ROOMS=128
KARBOSH_MAX_ROOM_CONNECTIONS=24
KARBOSH_MAX_WEBSOCKET_CONNECTIONS=256
EOF
chmod 600 ~/.config/karbosh/karbosh.env
```

The admin panel is available at `/karbosh/admin` and uses HTTP Basic Auth. The
default username is `admin`; override it with `KARBOSH_ADMIN_USER` if needed. If
`KARBOSH_ADMIN_PASSWORD` is unset, the admin panel returns a disabled response.
Use the admin panel through the HTTPS Apache proxy; do not send the admin
password over the temporary direct-HTTP `:8090` path.

The authenticated no-restart reload endpoint is:

```text
POST /karbosh/admin/reload
```

The room limits are guard rails, not game rules. Rooms can still run indefinitely,
but the process refuses new rooms or connections after the configured caps. The
defaults allow 128 rooms, 24 websocket connections per room, 256 total websocket
connections, and 8 KiB websocket messages.

Without the Apache proxy, the backend can temporarily serve the static client
directly:

```sh
KARBOSH_BIND=0.0.0.0 KARBOSH_PORT=8090 clojure -M:karbosh-server
```

That exposes the game at `http://dc3systems.com:8090/karbosh/`.

For a persistent service, install `karbosh.service` as a user or systemd unit
and set its working directory to the synced app directory.

The service includes modest hardening:

- `NoNewPrivileges=true` prevents the process and its children from gaining extra
  OS privileges.
- `UMask=0077` keeps any files the service creates private to the service user.
- `PrivateTmp=true` gives the service an isolated `/tmp`.
- `PrivateDevices=true` hides host device nodes from the service while keeping
  basic pseudo-devices available.
- `ProtectSystem=full` makes system directories such as `/usr` and `/etc`
  read-only to the service.
- `ProtectClock=true`, `ProtectHostname=true`, and `ProtectKernelLogs=true`
  stop the process from reading or changing host-level settings it does not need.
- the kernel/control-group restrictions block this game process from changing
  host-level kernel or cgroup settings.
- `RestrictAddressFamilies=AF_INET AF_INET6 AF_UNIX` leaves normal TCP and local
  socket use available while blocking unrelated protocol families.
- `CapabilityBoundingSet=` removes Linux capabilities from the process.
- `LimitNOFILE`, `TasksMax`, and `MemoryMax` provide simple blast-radius limits.

These settings do not block outbound/inbound network access or normal reads from
the project directory. Avoid `ProtectHome=true` here unless the Clojure and Maven
caches are moved elsewhere or explicitly allowed.
