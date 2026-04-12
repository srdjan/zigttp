# Deploy

Put a `handler.ts` in your project directory. Write and test it locally with `zigttp serve handler.ts`.

When you are ready to ship, run:

```
zigttp deploy
```

That is the whole command. No flags, no arguments, no config files, no cloud accounts to create, no registry to set up. On the first run, `zigttp deploy` prints a URL. Open it in a browser to sign in. After that, it remembers you. Future runs build your handler, upload it, and print the public URL where it is live.

## What it deploys

Your handler, and whatever it reads from `.env` in the current directory as runtime secrets. Nothing else. Zigttp figures out the rest by looking at your project:

- Handler file: the first match of `handler.ts`, `handler.tsx`, `handler.jsx`, `handler.js`, or the same under `src/`.
- Service name: the `name` field in `package.json`, or the basename of your git origin remote, or the current directory name. Slugified to lowercase with dashes.
- Runtime environment: key/value pairs in `.env` (one per line, `KEY=value`). Missing file is fine.

## Updates

Re-run `zigttp deploy` any time. Zigttp reuses the same service and only rolls out if the built image changed. Image references are content-addressed, so rebuilding identical code is a no-op.

## Sign out

```
zigttp logout
```

Forgets the saved credentials. The next `zigttp deploy` will prompt you to sign in again.
