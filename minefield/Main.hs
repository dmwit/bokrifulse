{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.IORef
import Data.Set (Set)
import Dr.Mario.Model
import ListT (ListT(ListT))
import System.Random.MWC
import qualified ListT as L
import qualified Data.Set as S

data ButtonPress
	= Move Direction
	| Rotate Rotation
	deriving (Eq, Ord, Show)

-- only works for short lists, but that's fine, we're only passing in a 5-element list
unsafeShuffle :: MonadIO m => GenIO -> [a] -> m [a]
unsafeShuffle gen xs = do
	let n = length xs
	order <- liftIO (uniformR (0, product [2..n]-1) gen)
	pure (go n order xs)
	where
	go 0 _ _ = []
	go n order xs = x : go (n-1) q (b++e) where
		(q,r) = order `quotRem` n
		(b, x:e) = splitAt r xs

shuffle :: GenIO -> [a] -> IO [a]
shuffle gen xs = go (length xs) xs where
	go :: Int -> [a] -> IO [a]
	-- 11! fits in 25 bits, and Haskell promises Int holds at least 29 bits, so
	-- unsafeShuffle is safe
	go n xs | n <= 12 = unsafeShuffle gen xs
	go n xs = let n' = n-1 in do
		ix <- uniformR (0, n') gen
		let (b, x:e) = splitAt ix xs
		(x:) <$> go n' (b++e)

buttonPress :: GenIO -> ListT IO ButtonPress
buttonPress gen = do
	moves <- unsafeShuffle gen [Move left, Move right, Move down, Rotate Clockwise, Rotate Counterclockwise]
	asum (map pure moves)

onFailure :: Monad m => m () -> ListT m a -> ListT m a
onFailure failAct (ListT act) = ListT $ do
	v <- act
	case v of
		Nothing -> failAct
		_ -> pure ()
	pure v

paths :: GenIO -> IORef (Set Pill) -> Board -> Pill -> Pill -> ListT IO [Pill]
paths gen deadRef b tgt = go S.empty False where
	go visited die src
		| src == tgt = pure [src]
		| y (bottomLeftPosition src) < y (bottomLeftPosition tgt) = empty
		| src `S.member` visited = empty
		| otherwise = do
			dead <- liftIO (readIORef deadRef)
			guard (src `S.notMember` dead)
			onFailure (when die (modifyIORef deadRef (S.insert src))) $ do
				press <- buttonPress gen
				src' <- maybe empty pure $ case press of
					Move d -> move b src d
					Rotate r -> rotate b src r
				(src:) <$> go (S.insert src visited) (press == Move down) src'

chooseTarget :: GenIO -> IO Pill
chooseTarget gen = do
	v <- uniformR (0, 59) gen
	let (px, py) = (v `rem` 32) `quotRem` 4
	pure Pill
		{ content = PillContent
			{ orientation = if v < 32 then Vertical else Horizontal
			, bottomLeftColor = Yellow
			, otherColor = Yellow
			}
		, bottomLeftPosition = Position { x = px, y = py }
		}

minfectRB :: IOBoard -> Position -> IO ()
minfectRB mb p = minfect mb p (if parity (x p) == parity (y p) then Red else Blue) where
	parity x = (x `quot` 2) `rem` 2

positions :: [Pill] -> Set Position
positions ps = S.fromList
	[ pos
	| p <- ps
	, pos <- [bottomLeftPosition p, otherPosition p]
	, y pos < 16
	]

findAmbiguities :: [Pill] -> ListT IO [Pill] -> IO ([Pill], Set Position, Set Position)
findAmbiguities ps = go where
	poss = positions ps
	go pss = L.uncons pss >>= \case
		Nothing -> pure (ps, S.empty, S.empty)
		Just (ps', pss') -> if S.null unique && S.null unique'
			then go pss'
			else pure (ps', unique, unique')
			where
			poss' = positions ps'
			unique = poss S.\\ poss'
			unique' = poss' S.\\ poss

canonicalSrc = Pill
	{ content = PillContent
		{ orientation = Horizontal
		, bottomLeftColor = Yellow
		, otherColor = Yellow
		}
	, bottomLeftPosition = Position { x = 3, y = 15 }
	}

constrain :: GenIO -> IOBoard -> Pill -> IO ()
constrain gen mb tgt = do
	deadRef <- newIORef S.empty
	b <- mfreeze mb
	Just solution <- L.head (paths gen deadRef b tgt canonicalSrc)
	go deadRef solution
	where
	go deadRef solution = do
		b <- mfreeze mb
		(solution', unique, unique') <- findAmbiguities solution (paths gen deadRef b tgt canonicalSrc)
		case (S.size unique, S.size unique') of
			(0, 0) -> pure ()
			(n, 0) -> minfectRandom unique  >> go deadRef solution'
			_      -> minfectRandom unique' >> go deadRef solution

	minfectRandom ps = do
		ix <- uniformR (0, S.size ps-1) gen
		minfectRB mb (S.elemAt ix ps)

deleteSuperfluousViruses :: GenIO -> IOBoard -> Pill -> IO ()
deleteSuperfluousViruses gen mb tgt = do
	b <- mfreeze mb
	viruses <- shuffle gen
		[ pos
		| x <- [0..7]
		, y <- [0..15]
		, let pos = Position x y
		, unsafeGet b pos /= Empty
		]
	for_ viruses $ \pos -> do
		deadRef <- newIORef S.empty
		mclear mb [pos]
		b <- mfreeze mb
		Just (ps, pss) <- L.uncons (paths gen deadRef b tgt canonicalSrc)
		(_, unique, unique') <- findAmbiguities ps pss
		unless (S.null unique && S.null unique') (minfectRB mb pos)

ppDead :: Set Pill -> String
ppDead = foldMap $ \p -> concat
	[ show . x . bottomLeftPosition $ p
	, if orientation (content p) == Horizontal then "-" else "|"
	, reverse . take 2 . reverse . ("0"++) . show . y . bottomLeftPosition $ p
	]

bokrifulseFormat :: Board -> Pill -> String
bokrifulseFormat b tgt = unlines $
	[ "version = 1"
	, "pill = ry"
	, "goal x = " ++ show (x (bottomLeftPosition tgt))
	, "goal y = " ++ show (y (bottomLeftPosition tgt))
	, "goal orientation = " ++ if orientation (content tgt) == Vertical then "1" else "0"
	] ++
	[ "row = " ++ [c | x <- [0..7], c <- fmtCell (unsafeGet b (Position x y))]
	| y <- [15,14..0]
	]
	where
	fmtCell Empty = "--"
	fmtCell (Occupied color shape) = [fmtColor color, fmtShape shape]
	fmtColor Red = 'r'
	fmtColor Blue = 'b'
	fmtColor Yellow = 'y'
	fmtShape Virus = 'x'
	fmtShape Disconnected = 'o'
	fmtShape North = '^'
	fmtShape South = 'v'
	fmtShape East = '<'
	fmtShape West = '>'

main :: IO ()
main = createSystemRandom >>= \gen -> do
	tgt <- chooseTarget gen
	mb <- memptyBoard 8 16
	constrain gen mb tgt
	deleteSuperfluousViruses gen mb tgt
	b <- munsafeFreeze mb
	putStr (bokrifulseFormat b tgt)
