# elm-review-elm-ui-upgrade

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to REPLACEME.

## Provided rules

- [`UpgradeElmUi`](https://package.elm-lang.org/packages/MartinSStewart/elm-review-elm-ui-upgrade/1.0.0/UpgradeElmUi) - Reports REPLACEME.

## Configuration

```elm
module ReviewConfig exposing (config)

import UpgradeElmUi
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ UpgradeElmUi.rule
    ]
```

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template MartinSStewart/elm-review-elm-ui-upgrade/example
```
