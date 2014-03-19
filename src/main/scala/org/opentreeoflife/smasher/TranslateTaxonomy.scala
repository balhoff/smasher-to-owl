package org.opentreeoflife.smasher

import scala.collection.JavaConversions._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import java.io.File

object TranslateTaxonomy extends App {

  val inputDirectory = args(0)
  val outFile = args(1)
  val manager = OWLManager.createOWLOntologyManager
  val taxonomy = new UnionTaxonomy()
  taxonomy.loadTaxonomy(inputDirectory)
  println("Converting nodes")
  val nodes = taxonomy.roots.toSet flatMap TaxonomyTranslator.translateWithDescendants
  println("Converting synonyms")
  val synonyms = TaxonomyTranslator.translateSynonyms(taxonomy.nameIndex)
  println("Building ontology")
  val ontology = manager.createOntology(nodes ++ synonyms)
  println("Writing ontology")
  manager.saveOntology(ontology, IRI.create(new File(outFile)))

}