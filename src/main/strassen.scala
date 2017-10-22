package main

object strassenMultiplication {
    //multiply two matrices with iterative method
    def multiplyMat(matrix1:Array[Array[Double]], matrix2:Array[Array[Double]]): Array[Array[Double]] = {
       val resultMat = Array.ofDim[Double](matrix1.length, matrix1(0).length)
        for (row <- 0 until matrix1.length) {
            for (k <- 0 until matrix2.length) {
                for (column <- 0 until matrix1(0).length) {
                    resultMat(row)(column) += matrix1(row)(k) * matrix2(k)(column)
                }
            }
        }
        return resultMat
    }
    
    //add two matrices
    def addMat(matrix1:Array[Array[Double]], matrix2:Array[Array[Double]]): Array[Array[Double]] = {
        val resultMat = Array.ofDim[Double](matrix1.length, matrix1(0).length)
        for (row <- 0 until matrix1.length) {
            for (column <- 0 until matrix1(0).length) {
                resultMat(row)(column) = matrix1(row)(column) + matrix2(row)(column)
            }
        }
        return resultMat
    }
    
    //subtract two matrices
    def subMat(matrix1:Array[Array[Double]], matrix2:Array[Array[Double]]): Array[Array[Double]] = {
        val resultMat = Array.ofDim[Double](matrix1.length, matrix1(0).length)
        for (row <- 0 until matrix1.length) {
            for (column <- 0 until matrix1(0).length) {
                resultMat(row)(column) = matrix1(row)(column) - matrix2(row)(column)
            }
        }
        return resultMat
    }
    
    //strassen multiplication
    def multiplyMatStras(matrix1:Array[Array[Double]], matrix2:Array[Array[Double]]): Array[Array[Double]] = {
        if (matrix1.length <= 2) {
            multiplyMat(matrix1, matrix2)
        }
        else {
            val half_len = matrix1.length / 2

            val A00 = Array.ofDim[Double](half_len, half_len)
            val A01 = Array.ofDim[Double](half_len, half_len)
            val A10 = Array.ofDim[Double](half_len, half_len)
            val A11 = Array.ofDim[Double](half_len, half_len)
            val B00 = Array.ofDim[Double](half_len, half_len)
            val B01 = Array.ofDim[Double](half_len, half_len)
            val B10 = Array.ofDim[Double](half_len, half_len)
            val B11 = Array.ofDim[Double](half_len, half_len)

            for (row <- 0 until half_len) {
                for (column <- 0 until half_len) {
                    A00(row)(column) = matrix1(row)(column)
                    A01(row)(column) = matrix1(row)(column + half_len)
                    A10(row)(column) = matrix1(row + half_len)(column)
                    A11(row)(column) = matrix1(row + half_len)(column + half_len)
                    
                    B00(row)(column) = matrix2(row)(column)
                    B01(row)(column) = matrix2(row)(column + half_len)
                    B10(row)(column) = matrix2(row + half_len)(column)
                    B11(row)(column) = matrix2(row + half_len)(column + half_len)
                }
            }
            
            //displayMat(A00)
            //displayMat(A01)
            //displayMat(A10)
            //displayMat(A11)

            val M1 = multiplyMatStras(addMat(A00, A11), addMat(B00, B11))
            val M2 = multiplyMatStras(addMat(A10, A11), B00)
            val M3 = multiplyMatStras(A00, subMat(B01, B11))
            val M4 = multiplyMatStras(A11, subMat(B10, B00))
            val M5 = multiplyMatStras(addMat(A00, A01), B11)
            val M6 = multiplyMatStras(subMat(A10, A00), addMat(B00, B01))
            val M7 = multiplyMatStras(subMat(A01, A11), addMat(B10, B11))
            
            val C00 = subMat(addMat(addMat(M1,M4),M7),M5)
            val C01 = addMat(M3, M5)
            val C10 = addMat(M2, M4)
            val C11 = subMat(addMat(addMat(M1, M3), M6),M2)

            val resultMat = Array.ofDim[Double](matrix1.length,matrix2(0).length)
            for (row <- 0 until half_len) {
                for (column <- 0 until half_len) {
                    resultMat(row)(column) = C00(row)(column)
                    resultMat(row)(column + half_len) = C01(row)(column)
                    resultMat(row + half_len)(column) = C10(row)(column)
                    resultMat(row + half_len)(column + half_len) = C11(row)(column)
                }
            }
            return resultMat
        }
    }

    //initializing the matrices
    def initMat(dim: Int): Array[Array[Double]] = {
      val newMat = Array.ofDim[Double](dim, dim)
      for (row <- 0 until newMat.length){
        for(column <- 0 until newMat(0).length){
          newMat(row)(column) = scala.util.Random.nextInt(100)
        }
      }
      return newMat
    }
    
    //displaying the matrix
    def displayMat(mat:Array[Array[Double]]){
      for (row <- 0 until mat.length) {
        for (column <- 0 until mat(0).length) {
          print(mat(row)(column)+" ")
        }
        println
      }
      println
    }
    
    //calculate time
    def time(proc: => Array[Array[Double]]):Array[Array[Double]] = {
        val start = System.currentTimeMillis
        val result = proc
        println("Time Taken:"+(System.currentTimeMillis - start) + "msec.")
        return result
    }
    
    //main function
    def main(args:Array[String]):Unit = {
      print("Enter the dimension of matrices: ")  
      val dimension: Int = scala.io.StdIn.readInt()
      //check if it is power of two
      if ((dimension & (dimension - 1)) == 0){
        val matrix1 = initMat(dimension)
        println("First Matrix:- ")
        displayMat(matrix1)
        val matrix2 = initMat(dimension)
        println("Second Matrix:- ")
        displayMat(matrix2)
        
        print("Multiplication (using Strassen Algorithm) => ")
        val result = time(multiplyMatStras(matrix1, matrix2))
        displayMat(result) 
        
        print("Multiplication (using Iterative method) => ")
        val result1 = time(multiplyMat(matrix1, matrix2))
        displayMat(result)
      }
      else{
        println("Dimension should be power of 2.")
      }
    }
}
