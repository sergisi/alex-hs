almostIncreasingSequence s = and $ (<2) . length . filter (uncurry (>=)) . zip s . tail <$> [s, tail s] ªº