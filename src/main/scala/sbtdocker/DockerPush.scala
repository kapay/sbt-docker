package sbtdocker

import sbt._

import scala.sys.process.{Process, ProcessLogger}

object DockerPush {
  /**
   * Push Docker images to a registry.
   *
   * @param dockerPath path to the docker binary
   * @param imageNames names of the images to push
   * @param log logger
   * @param auth credentials to authenticate to Docker registry
   */
  def apply(dockerPath: String, imageNames: Seq[ImageName], log: Logger, auth: Option[RegistryCredentials]): Unit = {
    imageNames.foreach { imageName =>
      apply(dockerPath, imageName, log, auth)
    }
  }

  /**
   * Push a Docker image to a registry.
   *
   * @param dockerPath path to the docker binary
   * @param imageName name of the image to push
   * @param log logger
   * @param auth credentials to authenticate to Docker registry
   */
  def apply(dockerPath: String, imageName: ImageName, log: Logger, auth: Option[RegistryCredentials]): Unit = {
    log.info(s"Pushing docker image with name: '$imageName'")

    val processLog = ProcessLogger({ line =>
      log.info(line)
    }, { line =>
      log.info(line)
    })

    /*var p = Process(dockerPath :: "info" :: Nil)
    p ! processLog*/

    auth match {
      case Some(a) =>
        val login = a.host match {
          case Some(h) =>
            dockerPath :: "login" :: "-u" :: a.userName :: "-p" :: a.password :: h :: Nil
          case None =>
            dockerPath :: "login" :: "-u" :: a.userName :: "-p" :: a.password :: Nil
        }

        log.debug(s"Running command: '${login.mkString(" ")}'")
        val loginProcess = Process(login)
        val loginExitValue = loginProcess ! processLog
        if (loginExitValue != 0) sys.error("Failed to login")

      case None =>
    }

    val command = dockerPath :: "push" :: imageName.toString :: Nil
    log.debug(s"Running command: '${command.mkString(" ")}'")

    val process = Process(command)
    val exitValue = process ! processLog
    if (exitValue != 0) sys.error("Failed to push")

    auth match {
      case Some(a) =>
        val logout = a.host match {
          case Some(h) =>
            dockerPath :: "logout" :: h :: Nil
          case None =>
            dockerPath :: "logout" :: Nil
        }

        val logoutProcess = Process(logout)
        val logoutExitValue = logoutProcess ! processLog
        if (logoutExitValue != 0) sys.error("Failed to logout")
      case None =>
    }
  }
}
