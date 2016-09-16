Net-A-Porter Shop Technical Exercise

You have been given a partially implemented e-Commerce application. We would like you to finish the implementation to provide some basic shopping features.
Running the Application

The Shop has been implemented as a command-line application. The entry point for the application is in Application.scala. The application uses SBT and can be run with the command:
sbt run
When running, the Application will shows a list of options, Application.scala has been implemented only to read the input commands.
We would like you to finish the application be completing the following scenarios.
Exercises

Exercise 1 - Loading and listing Products

We have provided a data file of Product information. In Exercise 1 you need too:

a) Parse the product-data.csv file to read the product data

b) Implement the "list" action to show which products can be purchased

Exercise 2 - Basic basket functionality

In this exercise you build on what you've done in Exercise 1. We would like you to implement some basic features of a Shopping Basket.

a) Implement the ability to add a product to the basket

b) Implement the ability to remove a product from the basket

c) Implement the ability to get the total value of the basket
