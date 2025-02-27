package probprog.expr

import probprog.distribution.WeightedSequence

import scala.math.exp
import scala.math.log
import scala.math.max
import scala.util.Random

case class ParticleExpr[T](particles: Vector[Particle[T]]) {
  def map[S](f: T => S): ParticleExpr[S] = ParticleExpr(particles.map(_.map(f)))

  def flatMap[S](f: T => ParticleExpr[S]): ParticleExpr[S] = {
    ParticleExpr(
      particles.map { p => {
        f(p.value).particles.map { psi => Particle(psi.value, p.logWeight + psi.logWeight) }
      }} .flatten)
  }

  def resample(n: Int, rnd: Random): ParticleExpr[T] = {
    val logWeights = particles.map(_.logWeight)
    val maxLogWeight = logWeights.reduce(max(_, _))
    val weights = logWeights.map { lwi => exp(lwi - maxLogWeight) }
    val law = maxLogWeight + log(weights.sum/weights.length)
    val dist = new WeightedSequence(particles.zip(weights))
    ParticleExpr(dist.sampleN(n, rnd).toVector.map { p => Particle(p.value, law) })
  }

  def ap[S](ff: ParticleExpr[T => S]): ParticleExpr[S] = {
    ParticleExpr(
      for {
        f <- ff.particles
        v <- particles
      } yield Particle(f.value(v.value), f.logWeight + v.logWeight)
    )
  }
}

object ParticleExpr {
  def pure[T](v: T): ParticleExpr[T] = ParticleExpr(Vector(Particle(v, 0.0)))
}

case class Particle[T](value: T, logWeight: Double) {
  def map[S](f: T => S): Particle[S] = Particle(f(value), logWeight)
}
