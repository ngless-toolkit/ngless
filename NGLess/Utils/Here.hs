module Utils.Here (here) where
import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | Heredocs
here = QuasiQuoter
    { quoteExp = stringE . skipStartNL
    , quotePat = const (error "cannot be used as pattern")
    , quoteType = const (error "cannot be used as type")
    , quoteDec = const (error "cannot be used as declaration")
    }

skipStartNL ('\n':rest) = rest
skipStartNL s = s
