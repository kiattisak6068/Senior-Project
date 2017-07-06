package utils

import java.io.File

object FileUtils {

    def listFiles(dir: File): Array[File] = {
      val these = dir.listFiles
      these ++ these.filter(_.isDirectory).flatMap(listFiles)
    }

}
