import $file.runtime

object macros extends runtime.MacrosModule

object library extends runtime.LibraryModule {
  override def macrosObject = macros
}