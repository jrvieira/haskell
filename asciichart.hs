import Data.Text.Chart (plot)

main :: IO ()
main = plot $ round.(*10).log <$> [1..99]
