module Model.Card(copper, silver, gold) 

where

import Model.Dominion
import Model.Game
import Control.Monad
import Prelude

moneyCard :: String -> Int -> Card
moneyCard n v = card n v noPoints noAction

copper :: Card
copper = moneyCard "Copper" 0
silver :: Card
silver = moneyCard "Silver" 2
gold :: Card
gold = moneyCard "Gold" 6

victory :: String -> Int -> Int -> Card
victory n c vp = card n c (\_ -> vp) noAction

estate :: Card
estate = victory "Estate" 2 1
duchy :: Card
duchy = victory "Duchy" 5 3
province :: Card
province = victory "Province" 8 6

curse :: Card
curse = card "Curse" 0 (return (-1)) noAction

noAction :: Player -> GamePlay ()
noAction p = return ()

noPoints :: [Card] -> Int
noPoints cards = 0

-- 
-- action cards --
--

-- Cellar Action  $2  +1 Action
-- Discard any number of cards.
-- +1 Card per card discarded.
cellar :: Card
cellar = card "Cellar" 2 noPoints cellarGamePlay
  where cellarGamePlay p = do
                            updateActions 1
                            cards <- cardsChoice "Choose cards to discard" p (cardsFromHand p)
                            forM cards (\c -> liftGame (discardCardFromHand c p))
                            forM cards (\c -> liftGame (drawCardAndPutInHand p))
                            return ()  


-- Chapel Action  $2  Trash up to 4 cards from your hand.
chapel :: Card
chapel = card "Chapel" 2 noPoints chapelGamePlay
  where chapelGamePlay p = do
                  cards <- cardsChoice "Choose cards to trash" p (maxCardsFromHand p 4)
                  forM cards (\c -> liftGame (discardCardFromHand c p))
                  return ()

-- Moat Action and Reaction $2  +2 Cards
-- When another player plays an Attack card, you may reveal this from your hand. If you do, you are unaffected by that Attack.
moat :: Card
moat = card "Moat" 2 noPoints moatGamePlay
  where moatGamePlay p = liftGame $ (drawCardAndPutInHand p) >> (drawCardAndPutInHand p)

-- Chancellor Action  $3  +$2
-- You may immediately put your deck into your discard pile.
chancellor :: Card
chancellor = card "Chancellor" 3 noPoints chancellorGamePlay
  where chancellorGamePlay p = do
          updateMoney 3
          deckInDiscardPile <- decision "Put deck into discard pile?" p
          if deckInDiscardPile then liftGame (putDeckOnDiscardPile p) else return ()
        putDeckOnDiscardPile p = do
          cnt <- drawSize p
          forM [1..cnt] (\_ -> drawAndDiscard p)
          return () :: Game Card ()
        drawAndDiscard p = do
          mc <- drawCard p
          maybe (return ()) (\c -> discardCard c p) mc 


-- Village  Action  $3  +1 Card; +2 Actions.
village :: Card
village = card "Village" 3 noPoints villageGamePlay
  where villageGamePlay p = (updateActions 2) >> (liftGame $ drawCardAndPutInHand p)


-- Woodcutter Action  $3  +1 Buy; +$2.
woodcutter :: Card
woodcutter = card "Woodcutter" 3 noPoints woodCutterGamePlay
  where woodCutterGamePlay p = (updateBuys 1) >> (updateMoney 2)


-- Workshop Action  $3  Gain a card costing up to $4.
workshop :: Card
workshop = card "Workshop" 3 noPoints workshopGamePlay
  where workshopGamePlay p = do
          cards <- cardsChoice "Choose a card costing up to $4" p (oneCardFromBoard 4)
          liftGame $ takeCardFromBoard (head cards)
          liftGame $ discardCard (head cards) p

oneCardFromBoard :: Int -> [Card] -> GamePlay (Maybe String)
oneCardFromBoard price cards = do
  cost <- liftGame $ boardCardValue (head cards)
  if length cards /= 1 
    then return $ Just "Please choose one card"
    else return $ cheap price cost
cheap price c =
  maybe (Just "Please choose a card from the board") 
        (\cost -> if cost <= price then Nothing else Just $ "Please choose a card costing up to $" ++ (show price)) 
        c

-- Bureaucrat Action � Attack $4  Gain a silver card; put it on top of your deck. Each other player reveals a Victory card from his hand and puts it on his deck (or reveals a hand with no Victory cards).
bureaucrat :: Card
bureaucrat = card "Bureaucrat" 4 noPoints bureaucratGamePlay
  where bureaucratGamePlay p = do
          liftGame $ (takeCardFromBoard silver) >> (putCardOnTopOfDeck silver p)
          attack undefined
          return ()

-- Feast  Action  $4  Trash this card. Gain a card costing up to $5.
feast :: Card
feast = card "Feast" 4 noPoints feastGamePlay
  where feastGamePlay p = do
          cards <- cardsChoice "Choose a card from board costing up to $5" p (oneCardFromBoard 5)
          liftGame $ takeCardFromBoard (head cards)
          liftGame $ discardCard (head cards) p
          liftGame $ takeCardFromTable feast
          return ()


--  Gardens Victory $4  Worth 1 Victory for every 10 cards in your deck (rounded down).
gardens :: Card
gardens = card "Gardens" 4 countPoints noAction
  where countPoints cards = (length cards) `div` 10
    

-- Militia  Action � Attack $4  +$2
-- Each other player discards down to 3 cards in his hand.
militia :: Card
militia = card "Militia" 4 noPoints militiaGamePlay
  where militiaGamePlay p = do
          updateMoney 2
          attack discardDownToThree
          return ()
        discardDownToThree p = do
          currentHand <- liftGame $ hand p
          cards <- cardsChoice "Choose cards to discard (down to 3 in hand)" p (\cs -> fmap (downToThree currentHand cs) (cardsFromHand p cs))
          forM cards (\c -> liftGame $ discardCardFromHand c p) 
          return ()
        downToThree h cs ms = if ms == Nothing then (if length h - length cs == 3 then Nothing else Just "Please discard down to 3") else ms
 
-- Moneylender  Action  $4  Trash a Copper  from your hand. If you do, +$3.
moneylender :: Card
moneylender = card "Moneylender" 4 noPoints moneyLenderGamePlay
  where moneyLenderGamePlay p = do
          trash <- decision "Trash a copper for $3?" p
          if trash then burnCopper p else return ()
        burnCopper p = do
          liftGame $ (takeCardFromHand copper p)
          updateMoney 3
          return ()   

-- Remodel  Action  $4  Trash a card from your hand. Gain a card costing up to $2 more than the trashed card.
remodel :: Card
remodel = card "Remodel" 4 noPoints remodelGamePlay
  where remodelGamePlay p = do
          cards <- cardsChoice "Choose card to thrash" p (maxCardsFromHand p 1)
          if cards == [] then return () else remodelCard (head cards) p
        remodelCard c p = do
          liftGame $ takeCardFromHand c p
          let price = cardValue c + 2
          cards <- cardsChoice ("Choose card from board costing up to " ++ (show price)) p (oneCardFromBoard price)
          forM cards (\c -> liftGame $ (takeCardFromBoard c >> discardCard c p))
          return ()
  
-- Smithy Action  $4  +3 Cards.
smithy :: Card
smithy = card "Smithy" 4 noPoints smithyGamePlay
  where smithyGamePlay p = liftGame $ drawCardAndPutInHand p >> drawCardAndPutInHand p >> drawCardAndPutInHand p

-- Spy  Action � Attack $4  +1 Card; +1 Action
-- Each player (including you) reveals the top card of his deck and either discards it or puts it back, your choice.
spy :: Card
spy = card "Spy" 4 noPoints spyGamePlay
  where spyGamePlay p = do
    updateActions 1
    liftGame $ drawCardAndPutInHand p
    card <- drawCard p
    
    discard <- decision "Discard " ++ (
    attack spyAttack
    


 