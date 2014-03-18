package org.opentreeoflife.smasher

import scala.collection.mutable
import scala.collection.JavaConverters._
import org.phenoscape.scowl.OWL._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLAxiom
import java.util.Collection
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.AxiomType
import org.semanticweb.owlapi.model.OWLLiteral

object TaxonomyTranslator {

  val factory = OWLManager.getOWLDataFactory
  val manager = OWLManager.createOWLOntologyManager
  val PREFIX = "http://purl.obolibrary.org/obo/OTT_"
  val xref = AnnotationProperty("http://www.geneontology.org/formats/oboInOwl#hasDbXref")
  val hasRelatedSynonym = AnnotationProperty("http://www.geneontology.org/formats/oboInOwl#hasRelatedSynonym")
  val hasRank = AnnotationProperty("http://purl.obolibrary.org/obo/TAXRANK_1000000")
  val ranks = manager.loadOntology(IRI.create("http://purl.obolibrary.org/obo/taxrank.owl"))
  val rankLabels = labelIndex(ranks)

  def translateNodes(nodes: Collection[Taxon]): Set[OWLAxiom] = (nodes.asScala flatMap translateNode).toSet

  def translateWithDescendants(node: Taxon): Set[OWLAxiom] = {
    val childrenAxioms = if (node.children != null) {
      (node.children.asScala flatMap translateWithDescendants).toSet
    } else Set[OWLAxiom]()
    childrenAxioms ++ translateNode(node)
  }

  def translateNode(node: Taxon): Set[OWLAxiom] = {
    if (node.id != null) {
      val axioms = mutable.Set[OWLAxiom]()
      val term = termForID(node.id)
      axioms += factory.getOWLDeclarationAxiom(term)
      for {
        parent <- Option(node.parent)
        parentID <- Option(parent.id)
      } {
        axioms += (term SubClassOf termForID(parentID))
      }
      if (node.name != null) {
        axioms += (term Annotation (factory.getRDFSLabel, node.name))
      }
      for (id <- node.sourceIds.asScala) {
        axioms += (term Annotation (xref, id.toString))
      }
      if (node.rank != Taxonomy.NO_RANK) {
        for {
          ranks <- rankLabels.get(node.rank)
          rank <- ranks.headOption
        } {
          axioms += factory.getOWLAnnotationAssertionAxiom(hasRank, term.getIRI, rank)
        }
      }
      axioms.toSet
    } else Set()
  }

  def translateSynonyms(nameIndex: java.util.Map[String, java.util.List[Taxon]]): Set[OWLAxiom] = {
    val axioms = for {
      (name, nodes) <- nameIndex.asScala
      node <- nodes.asScala
      if !node.prunedp
      if name != node.name
    } yield {
      termForID(node.id) Annotation (hasRelatedSynonym, name)
    }
    axioms.toSet
  }

  def termForID(id: String) = Class(PREFIX + id)

  def labelIndex(ont: OWLOntology): Map[String, Set[IRI]] = {
    val pairs = for {
      annotationAxiom <- ont.getAxioms(AxiomType.ANNOTATION_ASSERTION, false).asScala
      if annotationAxiom.getProperty == factory.getRDFSLabel
    } yield (annotationAxiom.getValue().asInstanceOf[OWLLiteral].getLiteral, annotationAxiom.getSubject().asInstanceOf[IRI])
    buildIndex(pairs)
  }

  def buildIndex[A, B](pairs: Iterable[(A, B)]): Map[A, Set[B]] =
    pairs.foldLeft(emptyIndex[A, B]()) { case (index, (a, b)) => index.updated(a, (index(a) + b)) }

  def emptyIndex[A, B](): Map[A, Set[B]] = Map().withDefaultValue(Set())

}