---
name: add-env
description: "Add an env var to the contract and handler. Usage: /template:add-env <VAR_NAME>"
---
Add the environment variable {{1}} to the handler. Confirm the live `zttp:env` export with `zts_expert_modules`, import only the needed function, avoid fallback secrets, and verify `no_secret_leakage` before applying the edit.
