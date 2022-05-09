import cats.effect._
import cats.effect.std.Console
import cats.syntax.all._
import cats.syntax.either._
import cats.data._
import cats.implicits._

implicit class I[A](val self: A) { def |>[B](f: A => B): B = f (self) }

/* Data model                                                                                                         */
/**********************************************************************************************************************/

object DataModel {
  import io.circe._
  import io.circe.generic.semiauto._

  implicit def eitherDecoder[A, B](implicit a: Decoder[A], b: Decoder[B]): Decoder[Either[A, B]] = { // https://stackoverflow.com/questions/61495083/how-to-create-a-decoder-for-an-either-type-with-circe
    val left:  Decoder[Either[A, B]] = a.map(Left.apply)
    val right: Decoder[Either[A, B]] = b.map(Right.apply)
    right.or (left)
  }

  def make_list[T] (v: Either[T, List[T]]): List[T] = v match {
    case Left (x) => x :: Nil
    case Right (xs) => xs
  }

  def make_list[T] (v: Option[Either[T, List[T]]]): List[T] = v.map (make_list).getOrElse (Nil)

  def parse_json_build_spec (str: String): IO[BuildSpecification] = for {
    build_spec_json_ <- SideEffect.readFile (str)
    build_spec_json = build_spec_json_.split("\n").filterNot (_.trim.startsWith ("//")).mkString("\n")
    build_spec0 <- IO {
      import io.circe.parser._
      decode[BuildSpecification] (build_spec_json) match {
        case Right (config) => config
        case Left (error) => throw new Exception (s"Failed to decode json: ${error.getMessage}")
      }
    }
    build_spec <- for {
      expanded_modules <- build_spec0.modules.map { m =>
        for {
          expanded_files <- m.files.map { f =>
            for {
              expanded_sources <- f.sources.map (SideEffect.wild).sequence.map (_.flatten)
              expanded_includes <- f.includes.map (SideEffect.wild).sequence.map (_.flatten)
              expanded_resources <- f.resources.map (SideEffect.wild).sequence.map (_.flatten)
            } yield f
              .copy (sources = expanded_sources)
              .copy (includes = expanded_includes)
              .copy (resources = expanded_resources)
          }.sequence

          expanded_rules <- m.rules.map { f =>
            for {
              expanded_pch_excludes <- f.pch_excludes.map (SideEffect.wild).sequence.map (_.flatten)
            } yield f
              .copy (pch_excludes = expanded_pch_excludes)
          }.sequence
        } yield m
          .copy (files = expanded_files)
          .copy (rules = expanded_rules)
      }.sequence
    } yield build_spec0.copy (modules = expanded_modules)

  } yield build_spec

  type Filepath                       = String
  type Wildpath                       = String
  type Dirpath                        = String
  type Filename                       = String
  type FileExtension                  = String

  type LanguageID                     = String
  type ConfigurationID                = String
  type ToolchainID                    = String
  type OperatingSystemID              = String
  type ModuleID                       = String
  type TargetID                       = String

  type CompilerIncludePath            = String
  type CompilerDefine                 = String
  type CompilerFlag                   = String
  
  type LinkerFlag                     = String
  type LinkerAdditionalLibrary        = String
  type LinkerLibrarySearchPath        = String

  type ScopeDeclaration               = Either[Scope, ScopeCombination]
  sealed trait LogicOp
  object LogicOp {
    object All  extends LogicOp { override def toString = "all" }
    object Any  extends LogicOp { override def toString = "any" }
    object None extends LogicOp { override def toString = "none" }

    def unapply (line: String): Option[LogicOp] = line match {
      case "all"  | "and" => Some (All)
      case "any"  | "or"  => Some (Any)
      case "none" | "not" => Some (None)
      case _ => _root_.scala.None
    }
    implicit val jd: Decoder[LogicOp] = Decoder.decodeString.emapTry { str =>
      scala.util.Try(unapply(str).get)
    }
  }

  sealed trait ModuleKind
  object ModuleKind {
    object Executable    extends ModuleKind { override def toString = "executable" }
    object StaticLibrary extends ModuleKind { override def toString = "static_library" }
    object SharedLibrary extends ModuleKind { override def toString = "shared_library" }
    object Interface     extends ModuleKind { override def toString = "interface" }

    def unapply (line: String): Option[ModuleKind] = line match {
      case "exe" | "executable" => Some (Executable)
      case "lib" | "static_library"  => Some (StaticLibrary)
      case "dll" | "shared_library" => Some (SharedLibrary)
      case "int" | "interface" => Some (Interface)
      case _ => _root_.scala.None
    }
    implicit val jd: Decoder[ModuleKind] = Decoder.decodeString.emapTry { str =>
      scala.util.Try(unapply(str).get)
    }
  }

  case class Scope (
    language                    : Option[LanguageID],
    configuration               : Option[ConfigurationID],
    toolchain                   : Option[ToolchainID],
    host                        : Option[OperatingSystemID],
    module                      : Option[ModuleID],
    target                      : Option[TargetID])
  object Scope {
    implicit val jd: Decoder[Scope] = deriveDecoder[Scope]
    lazy val default = Scope (None, None, None, None, None, None)
  }

  case class ScopeCombination   (rule: LogicOp, params: List[ScopeDeclaration])
  object ScopeCombination {
    implicit val jd: Decoder[ScopeCombination] = deriveDecoder[ScopeCombination]
  }

  case class CompilerSettings   (
    include_paths               : List[CompilerIncludePath],
    defines                     : List[CompilerDefine],
    flags                       : List[CompilerFlag])
  object CompilerSettings       {
    implicit val jd: Decoder[CompilerSettings] = new Decoder[CompilerSettings] {
      final def apply(c: HCursor): Decoder.Result[CompilerSettings] = for {
        j_include_paths <- c.downField("include_paths").as[Option[List[String]]]
        j_defines       <- c.downField("defines"      ).as[Option[List[String]]]
        j_flags         <- c.downField("flags"        ).as[Option[List[String]]]
      } yield CompilerSettings (
        j_include_paths.getOrElse (Nil),
        j_defines.getOrElse (Nil),
        j_flags.getOrElse (Nil))
    }
    lazy val empty = CompilerSettings (Nil, Nil, Nil)
  }

  case class LinkerSettings     (
    flags                       : List[LinkerFlag],
    additional_libraries        : List[LinkerAdditionalLibrary],
    library_search_paths        : List[LinkerLibrarySearchPath])
  object LinkerSettings       {
    implicit val jd: Decoder[LinkerSettings] = new Decoder[LinkerSettings] {
      final def apply(c: HCursor): Decoder.Result[LinkerSettings] = for {
        //j_dependencies         <- c.downField("dependencies"        ).as[Option[List[String]]]
        j_flags                <- c.downField("flags"               ).as[Option[List[String]]]
        j_additional_libraries <- c.downField("additional_libraries").as[Option[List[String]]]
        j_library_search_paths <- c.downField("library_search_paths").as[Option[List[String]]]
      } yield LinkerSettings (
        j_flags.getOrElse (Nil),
        j_additional_libraries.getOrElse (Nil),
        j_library_search_paths.getOrElse (Nil))
    }
    lazy val empty = LinkerSettings (Nil, Nil, Nil)
  }

  case class ScopedBuildSettingGroup (
    scope                       : Option[ScopeDeclaration],
    propogate                   : Boolean,
    compiler                    : CompilerSettings,
    linker                      : LinkerSettings)
  object ScopedBuildSettingGroup       {
    implicit val jd: Decoder[ScopedBuildSettingGroup] = new Decoder[ScopedBuildSettingGroup] {
      final def apply(c: HCursor): Decoder.Result[ScopedBuildSettingGroup] = for {
        j_scope     <- c.downField("scope"    ).as[Option[ScopeDeclaration]]
        j_propogate <- c.downField("propogate").as[Option[Boolean]]
        j_compiler  <- c.downField("compiler" ).as[Option[CompilerSettings]]
        j_linker    <- c.downField("linker"   ).as[Option[LinkerSettings]]
      } yield ScopedBuildSettingGroup (
        j_scope,
        j_propogate.getOrElse (false),
        j_compiler.getOrElse (CompilerSettings.empty),
        j_linker.getOrElse (LinkerSettings.empty))
    }
    lazy val empty = ScopedBuildSettingGroup (None, false, CompilerSettings.empty, LinkerSettings.empty)
  }

  case class Language (
    id                          : LanguageID,
    name                        : String,
    extensions                  : List[FileExtension],
    settings                    : List[ScopedBuildSettingGroup])
  object Language       {
    implicit val jd: Decoder[Language] = new Decoder[Language] {
      final def apply(c: HCursor): Decoder.Result[Language] = for {
        j_id         <- c.downField("id"        ).as[String]
        j_name       <- c.downField("name"      ).as[String]
        j_extensions <- c.downField("extensions").as[List[String]]
        j_settings   <- c.downField("settings"  ).as[Option[Either[ScopedBuildSettingGroup, List[ScopedBuildSettingGroup]]]]
      } yield Language (
        j_id,
        j_name,
        j_extensions,
        j_settings |> make_list)
    }
  }

  case class OperatingSystem (
    id                          : OperatingSystemID,
    name                        : String)
  object OperatingSystem {
    implicit val jd: Decoder[OperatingSystem] = deriveDecoder[OperatingSystem]
  }

  case class Host (
    os                          : OperatingSystemID,
    settings                    : List[ScopedBuildSettingGroup]) { def id = s"$os" }
  object Host {
    implicit val jd: Decoder[Host] = new Decoder[Host] {
      final def apply(c: HCursor): Decoder.Result[Host] = for {
        j_os         <- c.downField("os"        ).as[OperatingSystemID]
        j_settings   <- c.downField("settings"  ).as[Option[Either[ScopedBuildSettingGroup, List[ScopedBuildSettingGroup]]]]
      } yield Host (
        j_os,
        j_settings |> make_list)
    }
  }

  case class Target (
    id                          : TargetID,
    nice_name                   : String,
    shared_library_pattern      : String,
    static_library_pattern      : String,
    executable_pattern          : String,
    settings                    : List[ScopedBuildSettingGroup])
  object Target {
    implicit val jd: Decoder[Target] = new Decoder[Target] {
      final def apply(c: HCursor): Decoder.Result[Target] = for {
        j_id                     <- c.downField("id"                    ).as[String]
        j_nice_name              <- c.downField("nice_name"             ).as[String]
        j_shared_library_pattern <- c.downField("shared_library_pattern").as[String]
        j_static_library_pattern <- c.downField("static_library_pattern").as[String]
        j_executable_pattern     <- c.downField("executable_pattern"    ).as[String]
        j_settings               <- c.downField("settings"              ).as[Option[Either[ScopedBuildSettingGroup, List[ScopedBuildSettingGroup]]]]
      } yield Target (
        j_id,
        j_nice_name,
        j_shared_library_pattern,
        j_static_library_pattern,
        j_executable_pattern,
        j_settings |> make_list)
    }
  }

  case class ToolchainVariant (
    host_os                     : OperatingSystemID,
    target                      : TargetID,
    compilers                   : Map[LanguageID, String],
    pch_compilers               : Map[LanguageID, String],
    archiver                    : String,
    linker                      : String,
    dynamic_linker              : String,
    additional_library_pattern  : String,
    library_search_path_pattern : String,
    compiler_include_pattern    : String,
    compiler_define_pattern     : String,
    include_pch_pattern         : Option[String],
    settings                    : List[ScopedBuildSettingGroup])
  object ToolchainVariant {
    implicit val jd: Decoder[ToolchainVariant] = new Decoder[ToolchainVariant] {
      final def apply(c: HCursor): Decoder.Result[ToolchainVariant] = for {
        j_host_os                     <- c.downField("host_os"                    ).as[String]
        j_target                      <- c.downField("target"                     ).as[String]
        j_compilers                   <- c.downField("compilers"                  ).as[Map[String, String]]
        j_pch_compilers               <- c.downField("pch_compilers"              ).as[Option[Map[String, String]]]
        j_archiver                    <- c.downField("archiver"                   ).as[String]
        j_linker                      <- c.downField("linker"                     ).as[String]
        j_dynamic_linker              <- c.downField("dynamic_linker"             ).as[String]
        j_additional_library_pattern  <- c.downField("additional_library_pattern" ).as[String]
        j_library_search_path_pattern <- c.downField("library_search_path_pattern").as[String]
        j_compiler_include_pattern    <- c.downField("compiler_include_pattern"   ).as[String]
        j_compiler_define_pattern     <- c.downField("compiler_define_pattern"    ).as[String]
        j_include_pch_pattern         <- c.downField("include_pch_pattern"        ).as[Option[String]]
        j_settings                    <- c.downField("settings"                   ).as[Option[Either[ScopedBuildSettingGroup, List[ScopedBuildSettingGroup]]]]
      } yield ToolchainVariant (
        j_host_os,
        j_target,
        j_compilers,
        j_pch_compilers.getOrElse (Map.empty),
        j_archiver,
        j_linker,
        j_dynamic_linker,
        j_additional_library_pattern,
        j_library_search_path_pattern,
        j_compiler_include_pattern,
        j_compiler_define_pattern,
        j_include_pch_pattern,
        j_settings |> make_list)
    }
  }

  case class Toolchain (
    id                          : ToolchainID,
    variants                    : List[ToolchainVariant],
    settings                    : List[ScopedBuildSettingGroup])
  object Toolchain {
    implicit val jd: Decoder[Toolchain] = new Decoder[Toolchain] {
      final def apply(c: HCursor): Decoder.Result[Toolchain] = for {
        j_id         <- c.downField("id"      ).as[String]
        j_variants   <- c.downField("variants").as[List[ToolchainVariant]]
        j_settings   <- c.downField("settings").as[Option[Either[ScopedBuildSettingGroup, List[ScopedBuildSettingGroup]]]]
      } yield Toolchain (
        j_id,
        j_variants,
        j_settings |> make_list)
    }
  }

  case class Configuration (
    id                          : ConfigurationID,
    nice_name                   : String,
    settings                    : List[ScopedBuildSettingGroup])
  object Configuration {
    implicit val jd: Decoder[Configuration] = new Decoder[Configuration] {
      final def apply(c: HCursor): Decoder.Result[Configuration] = for {
        j_id        <- c.downField("id"       ).as[String]
        j_nice_name <- c.downField("nice_name").as[String]
        j_settings  <- c.downField("settings" ).as[Option[Either[ScopedBuildSettingGroup, List[ScopedBuildSettingGroup]]]]
      } yield Configuration (
        j_id,
        j_nice_name,
        j_settings |> make_list)
    }
  }

  case class PreCompiledHeaderSpec (
    language_id                 : LanguageID,
    header_file                 : Filepath)
  object PreCompiledHeaderSpec {
    implicit val jd: Decoder[PreCompiledHeaderSpec] = deriveDecoder[PreCompiledHeaderSpec]
  }

  case class ScopedModuleFileGroup (
    scope                       : Option[ScopeDeclaration],
    sources                     : List[Filepath],
    includes                    : List[Filepath],
    pchs                        : List[PreCompiledHeaderSpec],
    resources                   : List[Filepath])
  object ScopedModuleFileGroup {
    implicit val jd: Decoder[ScopedModuleFileGroup] = new Decoder[ScopedModuleFileGroup] {
      final def apply(c: HCursor): Decoder.Result[ScopedModuleFileGroup] = for {
        j_scope     <- c.downField("scope"    ).as[Option[ScopeDeclaration]]
        j_sources   <- c.downField("sources"  ).as[Option[List[Wildpath]]]
        j_includes  <- c.downField("includes" ).as[Option[List[Wildpath]]]
        j_pchs      <- c.downField("pchs"     ).as[Option[List[PreCompiledHeaderSpec]]]
        j_resources <- c.downField("resources").as[Option[List[Wildpath]]]
      } yield ScopedModuleFileGroup (
        j_scope,
        j_sources.getOrElse (Nil),
        j_includes.getOrElse (Nil),
        j_pchs.getOrElse (Nil),
        j_resources.getOrElse (Nil))
    }
  }

  case class ScopedModuleRuleGroup (
    scope                       : Option[ScopeDeclaration],
    pch_excludes                : List[Filepath], // sources to disable pchs usage for
    exported_inc_dirs           : List[Dirpath],
    dependencies                : List[ModuleID]) // to artifacts of this build system
  object ScopedModuleRuleGroup {
    implicit val jd: Decoder[ScopedModuleRuleGroup] = new Decoder[ScopedModuleRuleGroup] {
      final def apply(c: HCursor): Decoder.Result[ScopedModuleRuleGroup] = for {
        j_scope             <- c.downField("scope"            ).as[Option[ScopeDeclaration]]
        j_pch_excludes      <- c.downField("pch_excludes"     ).as[Option[List[Wildpath]]]
        j_exported_inc_dirs <- c.downField("exported_inc_dirs").as[Option[List[Dirpath]]]
        j_dependencies      <- c.downField("dependencies"     ).as[Option[List[ModuleID]]]
      } yield ScopedModuleRuleGroup (
        j_scope,
        j_pch_excludes.getOrElse (Nil),
        j_exported_inc_dirs.getOrElse (Nil),
        j_dependencies.getOrElse (Nil))
    }
  }

  case class Module (
    id                          : ModuleID,
    kind                        : ModuleKind,
    files                       : List[ScopedModuleFileGroup],
    rules                       : List[ScopedModuleRuleGroup],
    constrain_to                : Option[ScopeDeclaration],
    exclude_from                : Option[ScopeDeclaration],
    settings                    : List[ScopedBuildSettingGroup])
  object Module {
    implicit val jd: Decoder[Module] = new Decoder[Module] {
      final def apply(c: HCursor): Decoder.Result[Module] = for {
        j_id           <- c.downField("id"          ).as[String]
        j_kind         <- c.downField("kind"        ).as[ModuleKind]
        j_files        <- c.downField("files"       ).as[Either[ScopedModuleFileGroup, List[ScopedModuleFileGroup]]]
        j_rules        <- c.downField("rules"       ).as[Option[Either[ScopedModuleRuleGroup, List[ScopedModuleRuleGroup]]]]
        j_constrain_to <- c.downField("constrain_to").as[Option[ScopeDeclaration]]
        j_exclude_from <- c.downField("exclude_from").as[Option[ScopeDeclaration]]
        j_settings     <- c.downField("settings"    ).as[Option[Either[ScopedBuildSettingGroup, List[ScopedBuildSettingGroup]]]]
      } yield Module (
        j_id,
        j_kind,
        j_files |> make_list,
        j_rules |> make_list,
        j_constrain_to,
        j_exclude_from,
        j_settings |> make_list)
    }
  }

  case class BuildOutputConfig (
    ninja_directory             : Dirpath,
    binary_directory            : Dirpath,
    static_library_directory    : Dirpath,
    object_file_directory       : Dirpath) {
    def ninja_dir = ninja_directory
    def bin_dir = binary_directory
    def lib_dir = static_library_directory
    def obj_dir = object_file_directory
  }
  object BuildOutputConfig {
    implicit val jd: Decoder[BuildOutputConfig] = new Decoder[BuildOutputConfig] {
      final def apply(c: HCursor): Decoder.Result[BuildOutputConfig] = for {
        j_ninja_directory          <- c.downField("ninja_directory"       ).as[Option[String]]
        j_binary_directory         <- c.downField("binary_directory"        ).as[Option[String]]
        j_static_library_directory <- c.downField("static_library_directory").as[Option[String]]
        j_object_file_directory    <- c.downField("object_file_directory"   ).as[Option[String]]
      } yield BuildOutputConfig (
        j_ninja_directory.getOrElse ("_ninja"),
        j_binary_directory.getOrElse ("_bin"),
        j_static_library_directory.getOrElse ("_lib"),
        j_object_file_directory.getOrElse ("_obj"))
    }
    lazy val default = BuildOutputConfig ("_ninja", "_bin", "_lib", "_obj")
  }

  case class BuildSpecification (
    name                        : String,
    languages                   : List[Language],
    operating_systems           : List[OperatingSystem],
    hosts                       : List[Host],
    targets                     : List[Target],
    toolchains                  : List[Toolchain],
    configurations              : List[Configuration],
    modules                     : List[Module],
    settings                    : List[ScopedBuildSettingGroup],
    output_config               : BuildOutputConfig)
  object BuildSpecification {
    implicit val jd: Decoder[BuildSpecification] = new Decoder[BuildSpecification] {
      final def apply(c: HCursor): Decoder.Result[BuildSpecification] = for {
        j_name              <- c.downField("name"             ).as[String]
        j_languages         <- c.downField("languages"        ).as[List[Language]]
        j_operating_systems <- c.downField("operating_systems").as[List[OperatingSystem]]
        j_hosts             <- c.downField("hosts"            ).as[List[Host]]
        j_targets           <- c.downField("targets"          ).as[List[Target]]
        j_toolchains        <- c.downField("toolchains"       ).as[List[Toolchain]]
        j_configurations    <- c.downField("configurations"   ).as[List[Configuration]]
        j_modules           <- c.downField("modules"          ).as[List[Module]]
        j_settings          <- c.downField("settings"         ).as[Option[Either[ScopedBuildSettingGroup, List[ScopedBuildSettingGroup]]]]
        j_output_config     <- c.downField("output_config"    ).as[Option[BuildOutputConfig]]
      } yield BuildSpecification (
        j_name,
        j_languages,
        j_operating_systems,
        j_hosts,
        j_targets,
        j_toolchains,
        j_configurations,
        j_modules,
        j_settings |> make_list,
        j_output_config.getOrElse (BuildOutputConfig.default))
    }
  }
}


/* Intermediate Model                                                                                                 
 ***********************************************************************************************************************
 * Takes the Data Model and packs it into convenient shapes for other systems to use.
 */
object IntermediateModel {

  case class BuildEnvironment (
    env_vars                                      : Map[String, String])

  case class BuildInstanceIdentifier (
    host_os                                       : DataModel.OperatingSystemID,
    toolchain_id                                  : DataModel.ToolchainID,
    target_id                                     : DataModel.TargetID,
    configuration_id                              : DataModel.ConfigurationID)

  case class Module (
    id                                            : DataModel.ModuleID,
    kind                                          : DataModel.ModuleKind,

    sources                                       : Map[DataModel.LanguageID, List[DataModel.Filepath]],
    pchs                                          : Map[DataModel.LanguageID, List[DataModel.Filepath]],
    resources                                     : List[DataModel.Filepath],

    pch_excludes                                  : List[DataModel.Filepath],
    exported_inc_dirs                             : List[DataModel.Dirpath],
    dependencies                                  : List[DataModel.ModuleID],

    settings                                      : List[DataModel.ScopedBuildSettingGroup])

  case class BuildInstanceSpecification (
    host_os                                       : DataModel.OperatingSystemID,
    host_settings                                 : List[DataModel.ScopedBuildSettingGroup],

    toolchain_id                                  : DataModel.ToolchainID,
    toolchain_settings                            : List[DataModel.ScopedBuildSettingGroup],

    toolchain_variant_compilers                   : Map[DataModel.LanguageID, String],
    toolchain_variant_pch_compilers               : Map[DataModel.LanguageID, String],
    toolchain_variant_archiver                    : String,
    toolchain_variant_linker                      : String,
    toolchain_variant_dynamic_linker              : String,
    toolchain_variant_additional_library_pattern  : String,
    toolchain_variant_library_search_path_pattern : String,
    toolchain_variant_compiler_include_pattern    : String,
    toolchain_variant_compiler_define_pattern     : String,
    toolchain_variant_include_pch_pattern         : Option[String],
    toolchain_variant_settings                    : List[DataModel.ScopedBuildSettingGroup],

    target_id                                     : DataModel.TargetID,
    target_nice_name                              : String,
    target_shared_library_pattern                 : String,
    target_static_library_pattern                 : String,
    target_executable_pattern                     : String,
    target_settings                               : List[DataModel.ScopedBuildSettingGroup],

    configuration_id                              : DataModel.ConfigurationID,
    configuration_nice_name                       : String,
    configuration_settings                        : List[DataModel.ScopedBuildSettingGroup],

    languages                                     : Map[DataModel.LanguageID, DataModel.Language],
    modules                                       : Map[DataModel.ModuleID, Module],

    settings                                      : List[DataModel.ScopedBuildSettingGroup],

    output_config                                 : DataModel.BuildOutputConfig) {

    lazy val common_settings: List[DataModel.ScopedBuildSettingGroup] =
      host_settings :::
      toolchain_settings :::
      toolchain_variant_settings :::
      target_settings :::
      configuration_settings :::
      settings

    def all_dependencies_for (module_id: DataModel.ModuleID): List[DataModel.ModuleID] = {
      val rs = modules.get (module_id).map (_.dependencies).getOrElse (Nil)
      (rs ::: rs.flatMap { m_id => all_dependencies_for (m_id).filterNot (rs.contains) }).distinct
    }

    def all_additional_inc_dirs_for (module_id: DataModel.ModuleID): List[DataModel.Dirpath] = {
      modules.get (module_id).map (_.exported_inc_dirs).getOrElse (Nil) :::
      all_dependencies_for (module_id).map (modules.get).collect { case Some (x) => x }.flatMap (_.exported_inc_dirs).distinct
    }

    def all_settings_for (module_id: DataModel.ModuleID, language_id_opt: Option[DataModel.LanguageID] = None): List[DataModel.ScopedBuildSettingGroup] = {
      import extensions._
      (common_settings :::
      language_id_opt.map (language_id => languages.get (language_id).map (_.settings).getOrElse (Nil)).getOrElse(Nil) :::
      modules.get (module_id).map (_.settings).getOrElse (Nil) :::
      all_dependencies_for (module_id).map (modules.get).collect { case Some (x) => x }.flatMap (_.settings).filter (_.propogate)
      ).filter_for (module_id, language_id_opt)
    }

    def all_compiler_defines_for (module_id: DataModel.ModuleID, language_id: DataModel.LanguageID): List[DataModel.CompilerFlag] = 
      all_settings_for (module_id, Some (language_id)).flatMap { build_setting_group =>
        build_setting_group.compiler.defines
      }.distinct

    def all_compiler_flags_for (module_id: DataModel.ModuleID, language_id: DataModel.LanguageID): List[DataModel.CompilerFlag] = 
      all_settings_for (module_id, Some (language_id)).flatMap { build_setting_group =>
        build_setting_group.compiler.flags
      }.distinct

    def all_compiler_include_paths_for (module_id: DataModel.ModuleID, language_id: DataModel.LanguageID): List[DataModel.CompilerFlag] = 
      all_settings_for (module_id, Some (language_id)).flatMap { build_setting_group =>
        build_setting_group.compiler.include_paths
      }.distinct


    def all_linker_flags_for (module_id: DataModel.ModuleID): List[DataModel.LinkerFlag] = 
      all_settings_for (module_id).flatMap { build_setting_group =>
        build_setting_group.linker.flags
      }.distinct

    def all_linker_additional_libraries_for (module_id: DataModel.ModuleID): List[DataModel.LinkerAdditionalLibrary] = 
      all_settings_for (module_id).flatMap { build_setting_group =>
        build_setting_group.linker.additional_libraries
      }.distinct

    def all_linker_library_search_paths_for (module_id: DataModel.ModuleID): List[DataModel.LinkerLibrarySearchPath] = 
      all_settings_for (module_id).flatMap { build_setting_group =>
        build_setting_group.linker.library_search_paths
      }.distinct
  }

  object extensions {

    extension (scope_decl: DataModel.ScopeDeclaration) {
      def is_satisfied_for (
        instance_opt: Option[BuildInstanceIdentifier],
        module_id_opt: Option[DataModel.ModuleID],
        language_id_opt: Option[DataModel.LanguageID]): Boolean =
        scope_decl match {
          case Left (DataModel.Scope (languageOpt, configurationOpt, toolchainOpt, hostOpt, moduleOpt, targetOpt)) =>
            language_id_opt.map   { language_id => languageOpt.map      (_ == language_id).getOrElse (true)                       }.getOrElse (true) &&
            module_id_opt.map     { module_id   => moduleOpt.map        (_ == module_id).getOrElse (true)                         }.getOrElse (true) &&
            instance_opt.map      { instance    => configurationOpt.map (_.contains (instance.configuration_id)).getOrElse (true) }.getOrElse (true) &&
            instance_opt.map      { instance    => toolchainOpt.map     (_.contains (instance.toolchain_id)).getOrElse (true)     }.getOrElse (true) &&
            instance_opt.map      { instance    => hostOpt.map          (_.contains (instance.host_os)).getOrElse (true)          }.getOrElse (true) &&
            instance_opt.map      { instance    => targetOpt.map        (_.contains (instance.target_id)).getOrElse (true)        }.getOrElse (true)

          case Right (DataModel.ScopeCombination (rule, scope_decls)) =>
            val vx = scope_decls.map (_.is_satisfied_for (instance_opt, module_id_opt, language_id_opt))
            rule match {
              case DataModel.LogicOp.All => vx.reduce (_ && _)
              case DataModel.LogicOp.Any => vx.reduce (_ || _)
              case DataModel.LogicOp.None => vx match {
                case head :: Nil => !head
                case all => all.reduce (!_ && !_)
              }
            }
        }

      def is_satisfied_for (instance: BuildInstanceIdentifier): Boolean = is_satisfied_for (Some (instance), None, None)
      def is_satisfied_for (module_id: DataModel.ModuleID): Boolean = is_satisfied_for (None, Some (module_id), None)
      def is_satisfied_for (module_id: DataModel.ModuleID, language_id: DataModel.LanguageID): Boolean = is_satisfied_for (None, Some (module_id), Some (language_id))
    }

    extension (scoped_setting_group: DataModel.ScopedBuildSettingGroup) {
      def is_satisfied_for (instance: BuildInstanceIdentifier)
      : Boolean = scoped_setting_group.scope match {
        case None => true
        case Some (scope_decl) => scope_decl.is_satisfied_for (instance)
      }

      def expand_env_vars (env_vars: Map[String, String]): DataModel.ScopedBuildSettingGroup = {
        import Utils._

        DataModel.ScopedBuildSettingGroup (
          scoped_setting_group.scope,
          scoped_setting_group.propogate,
          DataModel.CompilerSettings (
            scoped_setting_group.compiler.include_paths.map (x => x.replace_env_vars (env_vars).normalise),
            scoped_setting_group.compiler.defines.map (x => x.replace_env_vars (env_vars).normalise),
            scoped_setting_group.compiler.flags.map (x => x.replace_env_vars (env_vars).normalise)),
          DataModel.LinkerSettings (
            scoped_setting_group.linker.flags.map (x => x.replace_env_vars (env_vars).normalise),
            scoped_setting_group.linker.additional_libraries.map (x => x.replace_env_vars (env_vars).normalise),
            scoped_setting_group.linker.library_search_paths.map (x => x.replace_env_vars (env_vars).normalise)))
      }

      def is_satisfied_for (module_id: DataModel.ModuleID, language_id_opt: Option[DataModel.LanguageID])
      : Boolean = scoped_setting_group.scope match {
        case None => true
        case Some (scope_decl) => scope_decl.is_satisfied_for (None, Some (module_id), language_id_opt)
      }

    }

    extension (scoped_file_group: DataModel.ScopedModuleFileGroup) {
      def is_satisfied_for (instance: BuildInstanceIdentifier)
      : Boolean = scoped_file_group.scope match {
        case None => true
        case Some (scope_decl) => scope_decl.is_satisfied_for (instance)
      }
    }

    extension (scoped_rule_group: DataModel.ScopedModuleRuleGroup) {
      def is_satisfied_for (instance: BuildInstanceIdentifier)
      : Boolean = scoped_rule_group.scope match {
        case None => true
        case Some (scope_decl) => scope_decl.is_satisfied_for (instance)
      }
    }

    extension (settings: List[DataModel.ScopedBuildSettingGroup]) {
      def filter_for (instance: BuildInstanceIdentifier)
      : List[DataModel.ScopedBuildSettingGroup] = settings.filter (_.is_satisfied_for (instance))

      def filter_for (module_id: DataModel.ModuleID, language_id_opt: Option[DataModel.LanguageID] = None)
      : List[DataModel.ScopedBuildSettingGroup] = settings.filter (_.is_satisfied_for (module_id, language_id_opt))
    }

    extension (files: List[DataModel.ScopedModuleFileGroup]) {
      def filter_for (instance: BuildInstanceIdentifier)(implicit d: DummyImplicit)
      : List[DataModel.ScopedModuleFileGroup] = files.filter (_.is_satisfied_for (instance))
    }

    extension (rules: List[DataModel.ScopedModuleRuleGroup]) {
      def filter_for (instance: BuildInstanceIdentifier)(implicit d1: DummyImplicit, d2: DummyImplicit)
      : List[DataModel.ScopedModuleRuleGroup] = rules.filter (_.is_satisfied_for (instance))
    }

    extension (spec: DataModel.BuildSpecification) {
      def specialise_for (instance: BuildInstanceIdentifier)
      : ReaderT[IO, BuildEnvironment, Either[Exception, BuildInstanceSpecification]] = Kleisli {
        (environment: BuildEnvironment) => IO.pure {

          val host_opt              = spec.hosts.find (x => x.id == instance.host_os)
          val target_opt            = spec.targets.find (x => x.id == instance.target_id)
          val toolchain_opt         = spec.toolchains.find (_.id == instance.toolchain_id)
          val toolchain_variant_opt = toolchain_opt.flatMap (_.variants.find (v => v.host_os == instance.host_os && v.target == instance.target_id))
          val configuration_opt     = spec.configurations.find (x => x.id == instance.configuration_id)

          (host_opt, target_opt, toolchain_opt, toolchain_variant_opt, configuration_opt) match {
            case (Some(host), Some(target), Some(toolchain), Some(toolchain_variant), Some(configuration)) =>

              import Utils._

              val languages = toolchain_variant.compilers.keys.map { l_id =>
                spec.languages.find (_.id == l_id)
              }.collect { case Some (x) => x }
              .map { lang => lang.copy (settings = lang.settings.map (_.expand_env_vars (environment.env_vars)))}

              val output_postfix = s"${instance.toolchain_id}.${instance.target_id}.${instance.configuration_id}"
              val output_config = DataModel.BuildOutputConfig (
                Utils.join (spec.output_config.ninja_dir, output_postfix),
                Utils.join (spec.output_config.bin_dir, output_postfix),
                Utils.join (spec.output_config.lib_dir, output_postfix),
                Utils.join (spec.output_config.obj_dir, output_postfix))

              val modules = spec.modules
                .filter { m =>
                  val inc = m.constrain_to.map { scope_decl => scope_decl.is_satisfied_for (instance) }.getOrElse (true)
                  val exc = m.exclude_from.map { scope_decl => scope_decl.is_satisfied_for (instance) }.getOrElse (false)
                  inc && !exc
                }
                .map { x =>
                  val files = x.files.filter_for (instance)
                  val rules = x.rules.filter_for (instance)
                  Module (
                    id = x.id,
                    kind = x.kind,
                    sources = languages.map { language =>
                      (language.id, language.extensions.flatMap (ext => files.flatMap (_.sources).filter (_.endsWith(ext))).distinct)
                    }.toMap.filterNot((k, v) => v.isEmpty),
                    pchs = Map (), // todo
                    resources = files.flatMap (_.resources).distinct,
                    pch_excludes = rules.flatMap (_.pch_excludes).distinct,
                    exported_inc_dirs = rules.flatMap (_.exported_inc_dirs).distinct,
                    dependencies = rules.flatMap (_.dependencies).distinct,
                    settings = x.settings.map (_.expand_env_vars (environment.env_vars)).filter_for (instance))
                }

              Right (BuildInstanceSpecification (
                host_os                                         = host.id,
                host_settings                                   = host.settings.map (_.expand_env_vars (environment.env_vars)).filter_for (instance),

                toolchain_id                                    = toolchain.id,
                toolchain_settings                              = toolchain.settings.map (_.expand_env_vars (environment.env_vars)).filter_for (instance),

                toolchain_variant_compilers                     = toolchain_variant.compilers.map { (l_id, c) => (l_id, c.replace_env_vars (environment.env_vars).normalise) },
                toolchain_variant_pch_compilers                 = toolchain_variant.pch_compilers.map { (l_id, c) => (l_id, c.replace_env_vars (environment.env_vars).normalise) },
                toolchain_variant_archiver                      = toolchain_variant.archiver.replace_env_vars (environment.env_vars).normalise,
                toolchain_variant_linker                        = toolchain_variant.linker.replace_env_vars (environment.env_vars).normalise,
                toolchain_variant_dynamic_linker                = toolchain_variant.dynamic_linker.replace_env_vars (environment.env_vars).normalise,
                toolchain_variant_additional_library_pattern    = toolchain_variant.additional_library_pattern,
                toolchain_variant_library_search_path_pattern   = toolchain_variant.library_search_path_pattern,
                toolchain_variant_compiler_include_pattern      = toolchain_variant.compiler_include_pattern,
                toolchain_variant_compiler_define_pattern       = toolchain_variant.compiler_define_pattern,
                toolchain_variant_include_pch_pattern           = toolchain_variant.include_pch_pattern,
                toolchain_variant_settings                      = toolchain_variant.settings.map (_.expand_env_vars (environment.env_vars)).filter_for (instance),

                target_id                                       = target.id,
                target_nice_name                                = target.nice_name,
                target_shared_library_pattern                   = target.shared_library_pattern,
                target_static_library_pattern                   = target.static_library_pattern,
                target_executable_pattern                       = target.executable_pattern,
                target_settings                                 = target.settings.map (_.expand_env_vars (environment.env_vars)).filter_for (instance),

                configuration_id                                = configuration.id,
                configuration_nice_name                         = configuration.nice_name,
                configuration_settings                          = configuration.settings.map (_.expand_env_vars (environment.env_vars)).filter_for (instance),

                languages                                       = languages.map (x => (x.id, x)).toMap,
                modules                                         = modules.map (x => (x.id, x)).toMap,
                settings                                        = spec.settings.map (_.expand_env_vars (environment.env_vars)).filter_for (instance),
                output_config                                   = output_config))
 
            case _ =>
              Left (Exception(s"Urgh!"))
          }
        }
      }
    }
  }
}


/* Ninja file specification                                                                                           */
/**********************************************************************************************************************/

object Ninja {
  type ModuleID = String
  type BuildSpecification = Map[Option[ModuleID], File]
  type File = List[Declaration]
  trait Declaration
  object NewLine extends Declaration
  case class Comment (text: String) extends Declaration
  case class Include (filename: String) extends Declaration
  case class Subninja (filename: String) extends Declaration
  case class Variable (name: String, value: String) extends Declaration
  case class Rule (name: String, command: String, use_rspfile: Boolean = false, depfile: Option[String] = None) extends Declaration
  case class Build (outputs: List[String], rule: String, inputs: List[String], variables: List[(String, String)] = Nil, implicit_outputs: List[String] = Nil, implicit_inputs: List[String] = Nil) extends Declaration

  /********************************************************************************************************************/

  def stringify (f: File):Reader[(String), String] = Reader { (ninja_dir: String) =>
    f.collect {
      case NewLine => "\n"
      case Comment (c) => s"# $c\n"
      case Include (fn) => s"include ${fn}.ninja\n"
      case Subninja (fn) => s"subninja ${Utils.join (ninja_dir, fn + ".ninja")}\n"
      case Variable (n, v) => s"$n = $v\n"
      case Rule (n, c, rsp, df) => rsp match {
        case false => s"rule $n\n  command = $c\n"
        case true =>
          val c2 = c.replace ("$in", "@$out.rsp")
          s"rule $n\n  command = $c2\n  rspfile = $$out.rsp\n  rspfile_content = $$in\n"
      }
      case Build (ox, r, ix, vm, iox, iix) =>
        def escapeString (str: String) = str
          .replace ("$", "$$")
          .replace (":", "$:")
          .replace (" ", "$ ")
        val os = ox.map (_ |> escapeString).mkString (" ")
        val is = ix.map (_ |> escapeString).mkString (" $\n") + (iix match { case Nil => ""; case x => " | " + x.map (_ |> escapeString).mkString (" ") })
        val vs = vm.map { case (n, v) => s"  $n = $v" }.mkString ("\n")
        val ios = iox match { case Nil => ""; case x => " | " + x.map (_ |> escapeString).mkString (" ") }
        if (vm.size > 0)
          if (ix.size > 1) s"build $os$ios: $r $$\n$is\n$vs\n"
          else             s"build $os$ios: $r $is\n$vs\n"
        else
          if (ix.size > 1) s"build $os$ios: $r $$\n$is\n"
          else             s"build $os$ios: $r $is\n"
      case _ => ""
    }.mkString
  }

  def create (spec: IntermediateModel.BuildInstanceSpecification)
  : Reader[(String, Boolean, Boolean), BuildSpecification] = Reader { (working_dir: String, full_paths: Boolean, use_pch: Boolean) =>

    val root_variables: List[Declaration] =
      Comment ("Variables") ::
      (full_paths match {
        case false =>
          Variable ("bin_dir", spec.output_config.binary_directory) ::
          Variable ("lib_dir", spec.output_config.static_library_directory) ::
          Variable ("obj_dir", spec.output_config.object_file_directory) ::
          Nil
        case true =>
          Variable ("bin_dir", Utils.join (working_dir, spec.output_config.binary_directory)) ::
          Variable ("lib_dir", Utils.join (working_dir, spec.output_config.static_library_directory)) ::
          Variable ("obj_dir", Utils.join (working_dir, spec.output_config.object_file_directory)) ::
          Nil
      }) :::
      NewLine ::
      Nil

    def build_objects  (module_id: DataModel.ModuleID): List[Declaration] = spec.modules (module_id).sources.flatMap { case (language_id, sources) =>

      val additional_inc_dirs = spec.all_additional_inc_dirs_for (module_id)

      val compiler_defines = spec.all_compiler_defines_for (module_id, language_id)
      val compiler_flags = spec.all_compiler_flags_for (module_id, language_id)
      val compiler_include_paths = spec.all_compiler_include_paths_for (module_id, language_id) ::: additional_inc_dirs

      Variable(s"${language_id}_defines", compiler_defines.map (x => spec.toolchain_variant_compiler_define_pattern.replace ("${id}", x)).mkString (" ")) ::
      Variable(s"${language_id}_flags", compiler_flags.mkString (" ")) ::
      Variable(s"${language_id}_include_paths", compiler_include_paths.map (x => spec.toolchain_variant_compiler_include_pattern.replace ("${id}", x)).mkString (" ")) ::
      ((use_pch, spec.toolchain_variant_include_pch_pattern) match {
        case (true, Some(pattern)) => spec.modules (module_id).pchs.getOrElse (language_id, Nil).map { pch_source =>
         val pch_output = Utils.change_ext(pch_source, "pch").replace('/', '.')
         val pch = Utils.join(spec.output_config.obj_dir, s"${module_id}.${pch_output}")
         val include_pch_pattern = (pattern.replace ("${id}", pch))
         Variable(s"${language_id}_include_pch", include_pch_pattern)
        }
       case _ => Nil
      }) :::
      NewLine ::
      sources.map { source =>
        val input = source
        val output_ = Utils.change_ext (input, "o").replace('/', '.')
        val output = Utils.join (
          spec.output_config.obj_dir,
          s"${module_id}.${output_}")

        val skip_pch_for_this_source = spec.modules (module_id).pch_excludes.contains (source) // todo: regex match here 
        val (implicit_inputs_, extra_vars) = (use_pch, skip_pch_for_this_source) match {
          case (true, false) =>
            val ii: List[String] = spec.modules (module_id).pchs.getOrElse (language_id, Nil)
            .map { pch_source =>
              val pch_output = Utils.change_ext(pch_source, "pch").replace('/', '.')
              Utils.join(spec.output_config.obj_dir, s"${module_id}.${pch_output}")
            }
            (ii, Nil)
          case (true, true) =>
            (Nil, List (("cxx_include_pch" -> "")))
          case _ => (Nil, Nil)
        }

        def append_wd (xs: List[String]) = xs.map { x => full_paths match {
          case true => Utils.join (working_dir, x)
          case false => x
        }}

        val implicit_inputs = implicit_inputs_ |> append_wd
        val inputs = (input :: Nil) |> append_wd
        val outputs = (output :: Nil) |> append_wd

        Build (outputs, s"compile_$language_id", inputs, extra_vars, Nil, implicit_inputs)
      } ::: NewLine :: Nil
    }.toList
      
    def build_archive  (m_id: DataModel.ModuleID): List[Declaration] = spec.modules.get (m_id) match {
      case Some (module) if module.kind == DataModel.ModuleKind.StaticLibrary =>
        val inputs = module.sources.flatMap { case (language_id, sources) => sources }.map { source =>
          val i = Utils.change_ext (source, "o").replace ('/', '.')
          Utils.join (
            spec.output_config.obj_dir,
            s"${module.id}.${i}")
        }.toList
        val outputs = Utils.join (
          spec.output_config.lib_dir,
          spec.target_static_library_pattern.replace ("${id}", module.id)) :: Nil
        Build (outputs, "archive", inputs) :: Nil

      case _ => Nil
    }

    def build_pchs     (m_id: DataModel.ModuleID): List[Declaration] = Nil
    def build_binary   (m_id: DataModel.ModuleID): List[Declaration] = spec.modules.get (m_id) match {
      case Some (module) if module.kind == DataModel.ModuleKind.Executable || module.kind == DataModel.ModuleKind.SharedLibrary =>
        val (rule, pattern) = module.kind match {
          case DataModel.ModuleKind.Executable => ("link", spec.target_executable_pattern)
          case DataModel.ModuleKind.SharedLibrary | _ => ("link_dynamic", spec.target_shared_library_pattern)
        }
        val outputs = Utils.join (
          spec.output_config.bin_dir,
          pattern.replace ("${id}", module.id)) :: Nil
        val implicit_outputs = (module.kind, spec.target_id) match {
          case (DataModel.ModuleKind.SharedLibrary, "orbis") | (DataModel.ModuleKind.SharedLibrary, "prospero") =>
            Utils.join (spec.output_config.lib_dir, "lib${id}_stub.a".replace ("${id}", module.id)) ::
            Utils.join (spec.output_config.lib_dir, "lib${id}_stub_weak.a".replace ("${id}", module.id)) :: Nil
          case (DataModel.ModuleKind.SharedLibrary, "win64") =>
            Utils.join (spec.output_config.bin_dir, "lib${id}.lib".replace ("${id}", module.id)) :: Nil
          case _ => Nil
        }
        
        val linker_library_search_paths =  spec.all_linker_library_search_paths_for (module.id)
        val linker_additional_libraries =  spec.all_linker_additional_libraries_for (module.id)
        val linker_flags =  spec.all_linker_flags_for (module.id)

        val library_search_paths = linker_library_search_paths.map (spec.toolchain_variant_library_search_path_pattern.replace ("${id}", _)).mkString (" ")
        val additional_libraries = linker_additional_libraries.map (spec.toolchain_variant_additional_library_pattern.replace ("${id}", _)).mkString (" ")
        val flags = linker_flags.mkString (" ")

        val inputs =
          module.sources.flatMap { case (language_id, sources) => sources }.toList.map { source =>
            val i = Utils.change_ext (source, "o").replace('/', '.')
            Utils.join (spec.output_config.obj_dir, s"${module.id}.${i}")
          } :::
          spec.all_dependencies_for (module.id)
            .map (dep => spec.modules.get (dep))
            .collect { case Some(dep) if dep.kind == DataModel.ModuleKind.StaticLibrary => Utils.join (spec.output_config.lib_dir, spec.target_static_library_pattern.replace ("${id}", dep.id)) }
        
        val vars =
          ("flags", flags) ::
          ("additional_libraries", additional_libraries) ::
          ("library_search_paths", library_search_paths) :: Nil

        Build (outputs, rule, inputs, vars, implicit_outputs) :: Nil
        
      case _ => Nil
    }

    def copy_resources (m_id: DataModel.ModuleID): List[Declaration] = spec.modules.get (m_id) match {
      case Some (module) =>
        module.resources.map { r =>
          val inputs = r :: Nil
          val outputs = Utils.join (spec.output_config.bin_dir, Utils.filename(r)) :: Nil
          Build (outputs, "copy_resource", inputs)
        }
      case _ => Nil
    }

    def pad (decls: List[Declaration]): List[Declaration] = decls match {
      case Nil => Nil
      case xs => xs ::: NewLine :: Nil
    }

    def ninja_declarations_for_module (m_id: DataModel.ModuleID): List[Declaration] =
      (build_objects (m_id) |> pad) :::
      (build_archive (m_id) |> pad) :::
      (build_pchs (m_id) |> pad) :::
      (build_binary (m_id) |> pad) :::
      (copy_resources (m_id) |> pad)

    val rules: List[Declaration] =
      Comment ("Rules") ::
      spec.toolchain_variant_compilers.map { (language_id, compiler) => Rule (s"compile_$language_id", compiler, false, Some ("$out.d")) }.toList ::: NewLine ::
      spec.toolchain_variant_pch_compilers.map { (language_id, pch_compiler) => Rule (s"pch_$language_id", pch_compiler, false, Some ("$out.d")) }.toList ::: NewLine ::
      Rule ("archive",       spec.toolchain_variant_archiver) :: NewLine ::
      Rule ("link",          spec.toolchain_variant_linker, true) :: NewLine ::
      Rule ("link_dynamic",  spec.toolchain_variant_dynamic_linker, true) :: NewLine ::
      Rule ("copy_resource", """python -c "import shutil;shutil.copyfile('$in', '$out')"""") :: NewLine ::
      Nil

    val subninjas: List[Declaration] = 
      Comment ("Modules") ::
      (spec.modules.keys.toList.map { m_id => Subninja (s"${spec.toolchain_id}.${spec.target_id}.${spec.configuration_id}.${m_id}") }) :::
      NewLine :: NewLine :: Nil
    val root_file: (Option[ModuleID], File) = (None, root_variables ::: rules ::: subninjas)
    val sub_files: List[(Option[ModuleID], File)] = spec.modules.keys.toList.map { m_id => 
      (Some(m_id),
        (if (full_paths) Nil else Comment (s"Path specifications are relative to: ${working_dir}") :: NewLine :: Nil) :::
        ninja_declarations_for_module (m_id))
    }
    (root_file :: sub_files).toMap
  }
}


/* Utils                                                                                                              */
/**********************************************************************************************************************/

object Utils {

  def change_ext (path: String, newext: String) = path.contains('.') match {
    case true => path.take (path.lastIndexOf (".")) + s".$newext"
    case false => path + s".$newext"
  }

  def filename (path: String) = {
    val i = path.lastIndexOf ("/")
    path.substring (i+1)
  }

  def join (base: String, components: String*): String =
    components.foldLeft (base) { (a, i) => a + "/" + i } |> Utils.normalise


  extension (str: String) {

    def normalise: String = {
      val s = str.replace ('\\', '/').replaceAll ("[/]+", "/")
      s.takeRight (1) match { case "/" => s.dropRight(1); case _   => s }
    }

    def replace_env_vars (env_vars: Map[String, String]): String = env_vars
      .map { case (k, v) => (k, v) }
      .foldLeft (str){ case (a, (k, v)) => a.replace ("${" + k + "}", v) }

  }
}


/* Side Effects                                                                                                       */
/**********************************************************************************************************************/

object SideEffect { // All paths here are expected to use / not \\

  import scala.util.{Try, Success, Failure}
  import scala.util.matching.Regex

  import java.io.File
  import java.nio.file.Paths

  import org.apache.commons.io.FileUtils
  import org.apache.commons.io.filefilter.{WildcardFileFilter, TrueFileFilter}

  implicit class StringExtensions (val t: String) {
    def | = t.stripMargin ('|')
    def \ = t.replace ('/', '\\')
    def / = t.replace ('\\', '/')
    def ESC = t.replace ("(", "%28").replace (")", "%29")
  }

  private [SideEffect] def toFile (p: String): IO[File] = for {
    workingDir <- getWorkingDirectory
    f <- IO {
     val path = p./
     val f1 = new File (path)
     if (f1.getCanonicalPath./ == path) f1
     else new File (workingDir + path)
    }
  } yield (f)

  def envVars (): IO[Map[String, String]] = IO {
    import scala.jdk.CollectionConverters._
    System.getenv().asScala.toMap
  }

  def host (): IO[String] = IO { System.getProperty("os.name", "generic").toLowerCase(java.util.Locale.ENGLISH) }

  def joinPaths (a: String, b: String): IO[String] = IO (Paths.get (a, b).normalize.toString)

  def getWorkingDirectory: IO[String] = for {
    wd <- IO (new File (System.getProperty ("user.dir")).getCanonicalPath./ + "/")
    //_ <- Console[IO].println ("  Q> get working directory = " + wd)
  } yield wd

  def setWorkingDirectory (path: String): IO[Unit] = for {
    //_ <- Console[IO].println ("  Q> set working directory: " + path)
    _ <- IO (System.setProperty ("user.dir", path))
  } yield ()

  def adjustWorkingDirectory (path: String): IO[Unit] = for {
    abs <- IO { new File (path).isAbsolute }
    _ <- abs match {
      case true => IO.pure (())
      case false => for {
        wd <- getWorkingDirectory
        adjusted <- joinPaths (wd, path)
        //_ <- Console[IO].println (s"  Q> adjust working directory from: $wd to: $adjusted")
        _ <- setWorkingDirectory (adjusted)
        wd1 <- getWorkingDirectory

      } yield ()
    }
  } yield ()

  def deleteDirectory (path: String): IO[Unit] = toFile (path).flatMap (f => IO (FileUtils.deleteDirectory (f)))

  def createDirectory (path: String): IO[Unit] = IO { 
    val f = new File (new File (path).getCanonicalPath./)
    f.mkdirs ()
  }

  def deleteFile (path: String): IO[Unit] = for {
    f <- toFile (path)
    _ <- IO { FileUtils.forceDelete (f) }
  } yield ()

  def writeFile (path: String, content: String): IO[Unit] = for {
    f <- toFile (path)
    _ <- IO { FileUtils.writeStringToFile (f, content, "UTF-8") }
  } yield (())

  def readFile (path: String): IO[String] = for {
    f <- toFile(path)
    content <- IO { Try (FileUtils.readFileToString (f, "UTF-8")) match {
      case Success (text) => IO.pure(text)
      case Failure (ex) => IO.raiseError (throw new Exception (s"Failed to read file @ $path >> $ex}"))
    }}.flatten
  } yield (content)

  def touch (path: String): IO[Unit] = for {
    f <- toFile(path)
    _ <-IO { FileUtils.touch (f) }
  } yield ()

  def wild (pattern: String): IO[List[String]] = pattern.contains ("*") match {
    case true => {
      val (pathPattern, filePattern) = pattern.splitAt (pattern.lastIndexOf ('/') + 1)
      for {
        workingDir <- getWorkingDirectory
        f <- toFile(pathPattern)
        fileFilter <- IO { new WildcardFileFilter(filePattern) }
        col <- IO { FileUtils.listFiles (f, fileFilter, TrueFileFilter.INSTANCE) }
        arr <- IO { FileUtils.convertFileCollectionToFileArray (col).toList: List[File] }
        res <- IO { arr.map (_.getCanonicalPath./) }
      }
      yield res.map (_.replace (workingDir, ""))
    }
    case false => IO.pure (pattern :: Nil)
  }
}


/* λ Make                                                                                                             */
/**********************************************************************************************************************/

object LambdaMake extends IOApp {

  sealed trait Action; object Action {
    object Generate extends Action { override def toString = "gen" }
  }

  case class Args (
    host          : String,
    toolchain     : String,
    target        : Option[String], 
    configuration : String,
    action        : Option[Action],
    wd            : Option[String],
    full_paths    : Boolean,
    use_pch       : Boolean,
    spec_file     : Option[String])
  object Args { def default = Args ("*", "*", None, "*", None, None, false, false, None) }

  implicit val r_string: scopt.Read[Option[String]] = scopt.Read.reads { Option (_) }
  implicit val r_action: scopt.Read[Option[Action]] = scopt.Read.reads {
    case "gen" => Some (Action.Generate)
    case _ => None
  }

  val parser = new scopt.OptionParser[Args]("bt") {
    head("bt", "0.0.x")
    opt[String]         ("host"         ).action ((x, c) => c.copy (host          = x)).withFallback (() => "nt"       ).text ("The host OS.")
    opt[String]         ("toolchain"    ).action ((x, c) => c.copy (toolchain     = x)).withFallback (() => "clang"    ).text ("The toolchain to use.")
    opt[String]         ("configuration").action ((x, c) => c.copy (configuration = x)).withFallback (() => "debug"    ).text ("The build configuration to use.")
    opt[Option[String]] ("target"       ).action ((x, c) => c.copy (target        = x)).withFallback (() => Some("x86")).text ("The target to generate files for.")
    opt[Option[Action]] ("action"       ).action ((x, c) => c.copy (action        = x)).withFallback (() => None       ).text ("The action to perform.")
    opt[Option[String]] ("wd"           ).action ((x, c) => c.copy (wd            = x)).withFallback (() => Some("../")).text ("The working directory.")
    opt[Boolean]        ("full_paths"   ).action ((x, c) => c.copy (full_paths    = x)).withFallback (() => false      ).text ("Should generated files use full_paths.")
    opt[Boolean]        ("use_pch"      ).action ((x, c) => c.copy (use_pch       = x)).withFallback (() => false      ).text ("Should pre-compiled headers be enabled.")
    opt[Option[String]] ("spec_file"    ).action ((x, c) => c.copy (spec_file     = x)).withFallback (() => None       ).text ("The filename of the spec file.")
  }


  /* Main                                                                                                             */
  /********************************************************************************************************************/

  def run(args: List[String]): IO[ExitCode] = for {
    start_dir <- SideEffect.getWorkingDirectory
    _ <- Console[IO].println (s"  +> starting directory: ${start_dir}")

    arguments <- IO(parser.parse (args, Args.default).getOrElse (Args.default))
    _ <- Console[IO].println ("  *> host: " + arguments.host)
    _ <- Console[IO].println ("  *> toolchain: " + arguments.toolchain)
    _ <- Console[IO].println ("  *> action: " + arguments.action)
    _ <- Console[IO].println ("  *> target: " + arguments.target)
    _ <- Console[IO].println ("  *> wd: " + arguments.wd)
    _ <- Console[IO].println ("  *> full_paths: " + arguments.full_paths)
    _ <- Console[IO].println ("  *> use_pch: " + arguments.use_pch)
    _ <- Console[IO].println ("  *> spec_file: " + arguments.spec_file)
    _ <- arguments.wd match {
      case None => IO.pure(start_dir)
      case Some (target_working_dir) => SideEffect.adjustWorkingDirectory (target_working_dir)
    }

    ec <- (for {

      working_dir <- SideEffect.getWorkingDirectory
      _ <- Console[IO].println ("  +> working directory: " + working_dir)

      // Get the build specification file as a string
      build_spec <- DataModel.parse_json_build_spec (working_dir + arguments.spec_file.getOrElse("build_config.json"))
      env_vars <- SideEffect.envVars ()

      exit_code <- arguments.action match {
        case None | Some (Action.Generate) =>
          arguments.target
            .map (List(_))
            .getOrElse (build_spec.targets.map(_.id))
            .map { target_id =>
              val host_os = arguments.host
              val toolchain_id = arguments.toolchain
              val configuration_id = arguments.configuration
              val build_env = IntermediateModel.BuildEnvironment (env_vars)
              val build_instance_id = IntermediateModel.BuildInstanceIdentifier (host_os, toolchain_id, target_id, configuration_id)
              import IntermediateModel.extensions._
              for {
                r0 <- build_spec.specialise_for (build_instance_id).run (build_env)
                r1 <- r0 match {
                  case Right (specialised_build_spec) =>
                    val ninja_build_spec = Ninja.create (specialised_build_spec)
                      .run (working_dir, arguments.full_paths, arguments.use_pch)
                    val ninja_file_dir = Utils.join (working_dir, specialised_build_spec.output_config.ninja_dir)
                    ninja_build_spec
                      .map { case (ninja_file_id, ninja_file) =>
                        val ext = ninja_file_id match { case Some (id) => s".${id}"; case None => "" }
                        val ninja_file_path = Utils.join (ninja_file_dir, s"${arguments.toolchain}.$target_id.${arguments.configuration}${ext}.ninja")
                        for {
                          _ <- Console[IO].println (s"  +> ninja file directory: $ninja_file_dir")
                          _ <- Console[IO].println (s"  +> ninja file complete path: $ninja_file_path")
                          _ <- SideEffect.writeFile (ninja_file_path, (ninja_file |> Ninja.stringify).run(specialised_build_spec.output_config.ninja_dir))
                        } yield (true)
                      }
                      .toList
                      .sequence
                      .map (x => x.reduce(_ && _))
                  case Left (ex) => IO.pure { throw ex }
                }
              } yield r1
            }
            .sequence
            .map (x => x.reduce(_ && _))
            .map (x => x match { case true => ExitCode.Success; case false => ExitCode.Error })
      }

      // reset the working directory to the start directory
      _ <- SideEffect.setWorkingDirectory (start_dir)
      
    } yield exit_code)
      .handleErrorWith { t =>
        implicit class Implicit (val t: Throwable) {
          def stackTrace: String = {
            import java.io.{PrintWriter, StringWriter}
            val stackTrace = new StringWriter
            t.printStackTrace (new PrintWriter (stackTrace))
            stackTrace.toString
          }
        }
        for {
         _ <- Console[IO].errorln(s"Error caught: ${t.getMessage}")
         _ <- Console[IO].errorln(t.stackTrace)
         _ <- SideEffect.setWorkingDirectory (start_dir)
        } yield (ExitCode.Error)
      }
  } yield ec
}
