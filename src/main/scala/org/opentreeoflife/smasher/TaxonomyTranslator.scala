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
      val term = termForID(node.id)
      val subClassAxiom = for {
        parent <- Option(node.parent)
        parentID <- Option(parent.id)
      } yield (term SubClassOf termForID(parentID))
      val labelAxiom = Option(node.name) map (term Annotation (factory.getRDFSLabel, _))
      val xrefAxioms = for {
        id <- node.sourceIds.asScala.toSet[QualifiedId]
      } yield (term Annotation (xref, id.toString))
      val rankAxiom = for {
        assignedRank <- Option(node.rank)
        ranks <- rankLabels.get(assignedRank)
        rank <- ranks.headOption
      } yield factory.getOWLAnnotationAssertionAxiom(hasRank, term.getIRI, rank)
      subClassAxiom.toSet ++ labelAxiom.toSet ++ xrefAxioms ++ rankAxiom.toSet + factory.getOWLDeclarationAxiom(term)
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