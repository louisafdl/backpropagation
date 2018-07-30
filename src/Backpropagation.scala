import scala.util.Random
import scala.math.exp

// First we define types for neurons, layers and network
case class Neuron(var weights : List[Double], var delta : Double, var output : Double)

object Backpropagation {

  def initialize_network(n_inputs: Int, n_hidden: Int, n_outputs: Int): List[List[Neuron]] = {
    val hidden_layer = List.fill(n_hidden)(Neuron(List.fill(n_inputs+1)(Random.nextDouble()),0.0,0.0))
    val output_layer = List.fill(n_outputs)(Neuron(List.fill(n_hidden+1)(Random.nextDouble()),0.0,0.0))
    List(hidden_layer, output_layer)
  }

  def activate(weights: List[Double], inputs: List[Double]) : Double = {
    weights.reverse.head + ((weights.reverse.tail.reverse, inputs).zipped map (_ * _)).sum
  }

  def transfer(activation: Double) : Double = { 1.0 / (1.0 + exp(-activation)) }


  def activate_neuron(neuron: Neuron, row: List[Double]) : Double  = {
    neuron.output = transfer(activate(neuron.weights, row))
    neuron.output
  }

  def activate_layer(layer: List[Neuron], row: List[Double]) : List[Double] = {
    layer.map(neuron => activate_neuron(neuron, row))
  }


  def forward_propagate(network : List[List[Neuron]], row: List[Double]) : List[Double] = {
    if (network.length == 1) {
      activate_layer(network.head, row)
    } else {
      forward_propagate(network.tail, activate_layer(network.head, row))
    }
  }

  // TESTE ET APPROUVE JUSQUE LA

  def transfer_derivative(output: Double) : Double = { output * (1.0 - output)}


  def backward_propagate_error(network: List[List[Neuron]], expected: List[Double]): Unit = {
    network.zipWithIndex.reverse.foreach(layer => {
      val errors = if(layer._2 != network.length-1) layer._1.zipWithIndex.map(error => network(network.indexOf(layer)+1).map(neuron => neuron.weights(error._2)*neuron.delta).sum)
      else layer._1.zipWithIndex.map(neuron => expected(neuron._2) - neuron._1.output)
      layer._1.zipWithIndex.foreach(neuron => neuron._1.delta = errors(neuron._2)*transfer_derivative(neuron._1.output))
    })
  }




  def main(args: Array[String]): Unit = {

    /*val neuron1 = Neuron(List(0.1,0.3,0.7), 0, 0)
    val neuron2 = Neuron(List(0.4,0.6), 0, 0)
    val neuron3 = Neuron(List(0.2,0.8), 0, 0)

    val row = List(1.0,0.0,1.0)

    val network = List(List(neuron1), List(neuron2, neuron3))

    println(forward_propagate(network, row))*/

    /*print("\nTest backpropagation of error")
    network = [[{'output': 0.7105668883115941, 'weights': [0.13436424411240122, 0.8474337369372327, 0.763774618976614]}],
    [{'output': 0.6213859615555266, 'weights': [0.2550690257394217, 0.49543508709194095]}, {'output': 0.6573693455986976, 'weights': [0.4494910647887381, 0.651592972722763]}]]
    expected = [0, 1]
    backward_propagate_error(network, expected)
    for layer in network:
        print(layer)*/

    val neuron1 = Neuron(List(0.13436424411240122, 0.8474337369372327, 0.763774618976614), 0, 0.7105668883115941)
    val neuron2 = Neuron(List(0.2550690257394217, 0.49543508709194095), 0, 0.6213859615555266)
    val neuron3 = Neuron(List(0.4494910647887381, 0.651592972722763), 0, 0.6573693455986976)

    val expected = List(0.0,1.0)

    val network = List(List(neuron1), List(neuron2, neuron3))

    backward_propagate_error(network, expected)

    for (layer <- network){
      println(layer)
    }




  }

}
