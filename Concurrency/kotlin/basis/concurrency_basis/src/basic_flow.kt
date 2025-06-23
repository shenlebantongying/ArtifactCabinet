package async.basicflow

import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.flow
import kotlinx.coroutines.runBlocking


fun intGen(): Flow<Int> =
    flow {
        for (i in 1..100) {
            emit(i)
        }
    }

fun main() = runBlocking {
    intGen().collect { v -> println("$v") }
}
