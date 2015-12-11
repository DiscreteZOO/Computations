import io.duality.PersistableSet

/**
  * Created by katja on 05/12/15.
  */
object DynamicProperties {

  private def getDynamicProperties(set: Set[String], propertyType: PropertyType): PersistableSet[DynamicProperty] = {
    val setDP = new PersistableSet[DynamicProperty]
    set.foreach(name => setDP += new DynamicProperty(name, propertyType))
    setDP
  }

  private val booleans = Set(
      "is_arc_transitive", "is_asteroidal_triple_free", "is_bipartite", "is_cartesian_product", "is_cayley", "is_chordal", "is_circulant",
      "is_circular_planar", "is_distance_regular", "is_distance_transitive", "is_edge_transitive", "is_eulerian", "is_even_hole_free", "is_forest", "is_gallai_tree",
      "is_hamiltonian", "is_interval", "is_line_graph", "is_long_antihole_free", "is_long_hole_free", "is_odd_hole_free", "is_overfull", "is_perfect", "is_prime",
      "is_regular", "is_split", "is_strongly_regular", "is_tree", "is_vertex_transitive", "has_multiple_edges")

  private val rationals = Set("average_degree", "average_distance", "cluster_transitivity", "clustering_average", "density", "maximum_average_degree")

  private val integers = Set("chromatic_index", "chromatic_number", "clique_number", "connected_components_number", "diameter",
      "edge_connectivity", "fractional_chromatic_index", "genus", "girth", "number_of_loops", "odd_girth", "radius", "size",
      "spanning_trees_count", "szeged_index", "triangles_count", "treewidth", "vertex_connectivity", "wiener_index", "zagreb1_index",
      "zagreb2_index")

  private val doubles = Set("lovasz_theta")

  val booleanProperties = getDynamicProperties(booleans, PropertyType.BooleanPropertyType)
  val doubleProperties = getDynamicProperties(doubles, PropertyType.DoublePropertyType)
  val integerProperties = getDynamicProperties(integers, PropertyType.IntegerPropertyType)
  val rationalProperties = getDynamicProperties(rationals, PropertyType.RationalPropertyType)

}
