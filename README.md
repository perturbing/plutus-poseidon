# plutus-poseidon
A rough plutus poseidon implementation to show that it is not viable to do onchain.

The benchmark shows (do `nix run .#bench`) that
```bash
hash n poseidon inputs (size 31 bytes)

    n     Script size             CPU usage               Memory usage
  ----------------------------------------------------------------------
    2   80119 (489.0%)     25158630853 (251.6%)       115235110 (823.1%) 
    3   80159 (489.3%)     40212605426 (402.1%)       183220072 (1308.7%) 
    4   80200 (489.5%)     67247346504 (672.5%)       305417264 (2181.6%) 
    5   80240 (489.7%)     96203863858 (962.0%)       436277632 (3116.3%) 
    6   80280 (490.0%)    141418529781 (1414.2%)       640737618 (4576.7%) 
```