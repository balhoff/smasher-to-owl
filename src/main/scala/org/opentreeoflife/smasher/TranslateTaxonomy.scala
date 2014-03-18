package org.opentreeoflife.smasher

import scala.collection.JavaConversions._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import java.io.File

object TranslateTaxonomy extends App {

  val manager = OWLManager.createOWLOntologyManager
  val taxonomy = new UnionTaxonomy()
  taxonomy.loadTaxonomy("/Users/jim/Desktop/ott/")
  println("Converting nodes")
  val nodes = taxonomy.roots.toSet flatMap TaxonomyTranslator.translateWithDescendants
  println("Converting synonyms")
  val synonyms = TaxonomyTranslator.translateSynonyms(taxonomy.nameIndex)
  println("Building ontology")
  val ontology = manager.createOntology(nodes ++ synonyms)
  println("Writing ontology")
  manager.saveOntology(ontology, IRI.create(new File("/Users/jim/Desktop/ott.owl")))

}