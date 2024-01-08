# elm-review-elm-ui-upgrade

Upgrades your elm-ui dependency from elm-ui to elm-ui 2.

## Try it out

If you have an existing Ui.elm module, rename it to something else (`MyUi.elm` for example)

After that, run the following command:
```bash
npx elm-review --template MartinSStewart/elm-review-elm-ui-upgrade/preview --fix-all
```
*Don't remove the `--fix-all` flag. This rule needs to generate all fixes in a single pass in order for it to work.*
