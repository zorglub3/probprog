package probprog

import scalax.collection.edges.UnDiEdge
import scalax.collection.generic.AbstractUnDiEdge
import scalax.collection.generic.UnapplyLabel
import scalax.collection.generic.UnapplyLabeledEdge
import scalax.collection.immutable.{Graph => ImmutableGraph}
import scalax.collection.mutable.Builder
import scalax.collection.mutable.TypedGraphFactory
import scalax.collection.mutable.{Graph => MutableGraph}

case class Edge[E](
  override val source: Int,
  override val target: Int,
  label: E,
) extends AbstractUnDiEdge(source, target)

object EdgeSyntax {
  implicit final class UnDiEdgeImplicits(val source: Int) extends AnyVal {
    def ~(target: Int) = UnDiEdge[Int](source, target)
  }

  implicit final class EdgeImplicits(val e: UnDiEdge[Int]) extends AnyVal {
    def :+[E](label: E) = Edge[E](e.source, e.target, label)
  }
}

class GraphicalModel[E] {
  object EdgeMatchSyntax {
    type Label = E
    type Node = Int

    object :~ extends UnapplyLabeledEdge[Node, Edge[Label], Label] {
      protected def label(e: Edge[Label]): Label = e.label
    }

    object +: extends UnapplyLabel[Node, Label]
  }

  object ImmutableModel {
    type T = ImmutableGraph[Int, Edge[E]]

    def empty: T = from(List.empty, List.empty)

    def from(nodes: Iterable[Int], edges: Iterable[Edge[E]]): T =
      ImmutableGraph.from(nodes, edges)

    def newBuilder: Builder[Int, Edge[E], ImmutableGraph] = ImmutableGraph.newBuilder
  }

  object MutableGraph extends TypedGraphFactory[Int, Edge[E]] {
    type T = MutableGraph[Int, Edge[E]]
  }
}

