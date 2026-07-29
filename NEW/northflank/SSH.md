# SSH access to the Northflank GPU nodes

All access goes through the Northflank CLI (already installed and logged in;
team `tearedcoder`). Keys: `nf.sh up` generates `~/.ssh/id_northflank` and
registers it as a Northflank *SSH identity* once per machine — nothing else to
set up.

## Which node

`nf.sh` targets a node via env vars (defaults: `fda-test`/`fda-node`):

| Node | Select with | GPUs |
|---|---|---|
| `fda-test-e1/fda-node4` | `NF_PROJECT=fda-test-e1 NF_SERVICE=fda-node4` | 4× H100 80GB (live) |
| `fda-test-e1/fda-node8h2` | `NF_PROJECT=fda-test-e1 NF_SERVICE=fda-node8h2` | 8× H200 141GB |
| `fda-test*/fda-node` | `NF_PROJECT=fda-test\|-e1\|-w\|-nl` | 8× H100 (capacity-queued) |

## Interactive shell

```bash
cd NEW/northflank
NF_PROJECT=fda-test-e1 NF_SERVICE=fda-node4 ./nf.sh ssh
```

## Standard SSH tools (scp / rsync / sftp / VS Code)

Northflank has no public SSH endpoint; the CLI opens a local proxy that
tunnels to the container. Start it and leave it running:

```bash
NF_PROJECT=fda-test-e1 NF_SERVICE=fda-node4 ./nf.sh proxy
# prints e.g.:  SSH proxy started on endpoint: 127.24.22.93:39745
```

Then point any SSH tool at that endpoint as `root` with the key:

```bash
ssh   -i ~/.ssh/id_northflank -p 39745 root@127.24.22.93
scp   -i ~/.ssh/id_northflank -P 39745 file.txt root@127.24.22.93:/root/
rsync -e "ssh -i ~/.ssh/id_northflank -p 39745" -avz ./data/ root@127.24.22.93:/cache/data/
```

**VS Code / Cursor**: Remote-SSH → connect to `root@<proxy-ip>` port
`<proxy-port>`, IdentityFile `~/.ssh/id_northflank`. The endpoint changes per
proxy session, so update the host entry when you restart the proxy.

## What's on the node

- `/root/fda/NEW` — this repo's `NEW/` tree (pushed by `./nf.sh sync`; the
  remote `.venv` is preserved across syncs)
- `/cache` — 500 GB persistent volume (HF models, torchinductor cache);
  survives pause/resume and is shared between the nodes in `fda-test-e1`
- Env (`~/.fda_env`, auto-sourced in login shells): `HF_HOME=/cache/hf`,
  `PYTORCH_ALLOC_CONF=expandable_segments:True`, `UV_NO_SYNC=1`, plus
  `HF_TOKEN`/`WANDB_API_KEY` if they were set locally during `bootstrap`
- Run things with `uv run --no-sync ...` from `/root/fda/NEW`

## Everyday commands

```bash
./nf.sh test              # rsync + pytest on the node
./nf.sh run '<cmd>'       # rsync + run a command in /root/fda/NEW
./nf.sh status            # deployment state + nvidia-smi
./nf.sh pause             # ⚠ stop billing when idle; resume with ./nf.sh resume
```

(all honoring the `NF_PROJECT`/`NF_SERVICE` selectors above)

## Experiment 5

Experiment 5 is pinned to the on-demand Central four-H100 service. The
preflight runs the complete 12-cell Stage A graph first on one H100 and then
with four-way DDP, checks memory, optimizer state, finite loss, and at least
3.2× throughput scaling. It automatically retries with microbatch 64 when
the default 128 is rejected.

```bash
cd NEW/northflank
./nf.sh exp5-plan
NF_PROJECT=fda-test-e1 NF_SERVICE=fda-node4 ./nf.sh exp5-preflight
NF_PROJECT=fda-test-e1 NF_SERVICE=fda-node4 ./nf.sh exp5-launch
NF_PROJECT=fda-test-e1 NF_SERVICE=fda-node4 ./nf.sh exp5-status
NF_PROJECT=fda-test-e1 NF_SERVICE=fda-node4 ./nf.sh exp5-logs
```

`exp5-preflight` and `exp5-launch` check for GPU compute PIDs and existing
study or coordinator processes before rsync. If the node is busy, they exit
without syncing or launching. The H200 Spot node is deliberately rejected.

## Troubleshooting

- **"no TASK_RUNNING container"** — node is paused, still staging (capacity
  queue), or crashed: `./nf.sh status`.
- **Permission denied (publickey)** — your key isn't the registered identity;
  check `northflank list ssh-identities --teamId tearedcoder` and re-run
  `./nf.sh up` (or add your pubkey in Team → Integrations → SSH Identities).
- **`ssh: connect ... refused`** — the proxy from a previous session died;
  restart `./nf.sh proxy` and use the freshly printed endpoint.
