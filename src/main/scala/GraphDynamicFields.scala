import io.duality.PersistableSet

/**
  * Created by katja on 05/12/15.
  */
class GraphDynamicFields(values: Map[String, Any]) {

  val fieldNames = Set(
      // Boolean
      "is_arc_transitive", "is_asteroidal_triple_free", "is_bipartite", "is_cartesian_product", "is_cayley", "is_chordal", "is_circulant",
      "is_circular_planar", "is_distance_regular", "is_distance_transitive", "is_edge_transitive", "is_eulerian", "is_even_hole_free", "is_forest", "is_gallai_tree",
      "is_hamiltonian", "is_interval", "is_line_graph", "is_long_antihole_free", "is_long_hole_free", "is_odd_hole_free", "is_overfull", "is_perfect", "is_prime",
      "is_regular", "is_split", "is_strongly_regular", "is_tree", "is_vertex_transitive",
      // Rational
      "average_degree", "average_distance", "cluster_transitivity", "clustering_average", "density", "maximum_average_degree",
      // Integer
      "chromatic_index", "chromatic_number", "clique_number", "diameter", "edge_connectivity", "fractional_chromatic_index", "genus", "girth",
      // Double
      "lovasz_theta")

  val fields = new PersistableSet[DatabaseField[Any]]
  fieldNames.foreach(fieldName => fields += new DatabaseField(fieldName, values.get(fieldName)))

}
