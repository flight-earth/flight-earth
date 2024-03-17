\(_stackage-resolver : Optional Text) ->
  ''
  user-message: "WARNING: This stack project is generated."

  ghc-options:
      "$locals": -Werror=unused-imports

  flags:
      detour-via-sci:
          suppress-failing-tests: true
      detour-via-uom:
          suppress-failing-tests: true
      flight-earth:
          suppress-failing-tests: true
      flight-task:
          suppress-failing-tests: true
      flight-units:
          suppress-failing-tests: false
      flight-zone:
          suppress-failing-tests: true
  ''
