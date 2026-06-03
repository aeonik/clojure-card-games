# Karbosh Deployment

Static files deploy with the normal site rsync.

The multiplayer backend normally listens on `127.0.0.1:8090`. Apache needs to proxy
`/karbosh/ws` and `/karbosh/api/` to that process; see `apache-karbosh.conf`.
This step requires privileged access because the active virtual host lives under
`/etc/apache2/sites-available/`.

After syncing the app source to the server:

```sh
cd /var/www/dc3systems.com/apps/clojure-card-games
clojure -M:karbosh-server
```

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
