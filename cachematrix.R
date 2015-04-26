##逆矩阵通常是费时费力的计算，缓存逆矩阵可能会比对其进行重复计算更有利
##以下作业是编写可以缓存逆矩阵的函数对

## 第一个函数makeCacheMatrix创建了一个特殊的“矩阵”
##实际上它是包含具有以下用途的函数的列表
##1.设置矩阵的值
##2.获取矩阵的值
##3.设置逆矩阵的值
##4.获取逆矩阵的值

makeCacheMatrix <- function(x = matrix()) { 
                m <- NULL ##把m赋值为null
                set <- function(y) {
                        x <<- y 
                        m <<- NULL
                }
                get <- function() x ##把x赋值给get
                setsolve <- function(solve) m <<- solve ##这里求逆矩阵
                getsolve <- function() m ##这里把m复制给getsolve（NULL）
                list(set = set, get = get,
                     setsolve = setsolve,
                     getsolve = getsolve)  
}


##以下函数计算出了上述函数创建的特殊“矩阵”的逆矩阵。
##但是，它会首先查看是否已经计算了逆矩阵。
##如果是这种情况，那么它会从缓存中获取逆矩阵的值并跳过计算。
##否则，它会计算矩阵的逆矩阵并通过setsolve函数在缓存中设置逆矩阵。

cacheSolve <- function(x, ...) { { 
        m <- x$getsolve() ##首先把getsolve赋值给m
        if(!is.null(m)) { ##如果m不是NULL的话弹出下列提示
                message("getting cached data")
                return(m)
        }
        data <- x$get() ##把最开始的matrix()赋值给data
        m <- solve(data, ...)
        x$setsolve(m)
        m        ## Return a matrix that is the inverse of 'x'
}
