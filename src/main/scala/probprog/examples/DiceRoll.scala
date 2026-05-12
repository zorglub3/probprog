package probprog.examples

import probprog.Language

class DiceRoll(val lang: Language) {
  import lang._

  val goblinStrength = 2
  val goblinDexterity = 2

  def rollD6(): F[Int] = 
    sample(uniformSequence(List(1, 2, 3, 4, 5, 6)))

  def d6(n: Int): F[Int] =
    if(n == 0) {
      pure_(0)
    } else {
      for {
        x <- rollD6()
        r <- d6(n - 1)
      } yield x + r
    }

  def rollD6Advantage(): F[Int] =
    for {
      first <- rollD6()
      second <- rollD6()
    } yield first.max(second)

  def attackGoblin(strength: Int): F[Boolean] = 
    for {
      playerRoll <- rollD6()
      gmRoll <- rollD6()
    } yield (playerRoll + strength) >= (gmRoll + goblinDexterity) 

  def defendGoblin(dexterity: Int): F[Boolean] =
    for {
      playerRoll <- rollD6()
      gmRoll <- rollD6()
    } yield (playerRoll + dexterity) >= (gmRoll + goblinStrength) 

  // case class Player(strength: Int, dexterity: Int, willpower: Int)

  def round(str: Int, dex: Int): F[(Int, Int)] = // return (goblin loss, player loss)
    for {
      attackSuccess <- attackGoblin(str)
      defendSuccess <- defendGoblin(dex)
    } yield (if(attackSuccess) 1 else 0, if(defendSuccess) 0 else 1)

}

object DiceRoll {
  import probprog.util.Normalize

  val prog = new DiceRoll(new probprog.impl.LikelihoodWeight)

  def oneRound(str: Int, dex: Int) =
    Normalize(prog.lang.run(prog.round(str, dex), 10000))

  def roll[T](f: prog.lang.F[T]) =
    Normalize(prog.lang.run(f, 10000))
}
