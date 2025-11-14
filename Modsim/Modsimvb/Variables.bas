Attribute VB_Name = "Variables"
Option Explicit

Type OutFileFormat
  CoalFlag As Boolean
  Quantity(6) As Integer
  SolidUnits As Integer
  WaterUnits As Integer
  MetalUnits As Integer
  SizeDistributions As Boolean
  Nstream As Integer
  Stream(30) As Integer
  Accumulation As Boolean
  NAccStream As Integer
  AccStream(30) As Integer
  NumberMetals As Integer
  MetalName(4) As String * 8
  MinMetal(7, 4) As Single
End Type

Public Type ConversionFactor
  Number As Integer
  Factor(1 To 10) As Double
End Type
Public CFactor As ConversionFactor
Public PARAMIndex As Integer
Public m_cm_ft As ConversionFactor
Public m_feet As ConversionFactor
Public m_cm_inch As ConversionFactor
Public cm_m_inch As ConversionFactor
Public cm_m_inch_ft As ConversionFactor
Public mm_inch As ConversionFactor
Public mm_cm_m_inch As ConversionFactor
Public mm_cm_m As ConversionFactor
Public mm_cm_inch As ConversionFactor
Public micron_mm_cm_m_inch As ConversionFactor
Public micron_mm_cm_m_inch_ft As ConversionFactor
Public kgps_stonphr_tonnephr_ltonphr As ConversionFactor
Public kgps_tonnephr_stonphr_ltonphr As ConversionFactor
Public kgps_tonnephr_stonphr_Mtonnepyr As ConversionFactor
Public Pas_cp As ConversionFactor
Public cubm_cubft As ConversionFactor
Public cubmpers_cubmperhr_USgalpermin As ConversionFactor
Public kgps_cubmphr_cubftpmin_USgalpmin_ImpGalpmin As ConversionFactor
Public kgps_cubmphr_cubftpmin_USgalpmin_ImpGalpmin_Lpermin As ConversionFactor
Public cubmps_cubmphr_cubftpmin_USgalpmin_ImpGalpmin As ConversionFactor
Public kgpcubm_lbpcubft As ConversionFactor
Public gpercc_kgpcubm_lbpcubft As ConversionFactor
Public mps_ftpmin As ConversionFactor
Public kWhpert_kWhperst As ConversionFactor
Public cubmpkg_emupg As ConversionFactor

Public SavedJob As Boolean

Public JobFileName As String
Public JobFlowsheet As Boolean
Public JobSystemData As Boolean
Public JobModelData As Boolean
Public JobSystemInputData As Boolean
Public JobModelParameterData As Boolean
Public JobUnitParametersNeeded As Boolean
Public JobSystemDataNeeded As Boolean
Public JobCurrentData As Boolean
Public JobSizeData As Boolean
Public JobGCDistribution As Boolean
Public JobWashabilityData As Boolean
Public JobSCDistribution As Boolean
Public JobLibcomp As Boolean
Public JobOutputFormat As Boolean
Public JobSimopData As Boolean
Public JobLjubAMD As Boolean
Public JobBetaAMD As Boolean

Public ChangedJobname As Boolean
Public ChangedFlowsheet As Boolean
Public ChangedSystemInputData As Boolean
Public ChangedModelParameterData As Boolean
Public ChangedOutputFormat As Boolean
Public ChangedA_MDiagramParameters As Boolean
Public SimulationHasBeenRun As Boolean
Public FastParameterChange As Boolean
Public FastFeedStreamChange As Boolean
Public FastWaterStreamChange As Boolean
Public FastOtherStreamChange As Boolean
Public DontShowHelp As Boolean

Public DisplayOffsetX As Integer
Public DisplayOffsetY As Integer

Public Definedflowsheet As Boolean
Public NewFlowsheet As Boolean   'To prevent viewing flowsheet which has been changed and not simulated
Public DemoVersion As Boolean
Public StudentVersion As Boolean
Public BHVersion As Boolean
Public AcademicVersion As Boolean
Public ProfessionalVersion As Boolean
Public PreviousURL As String, PreviousUsername As String

Public OptionShowFlowsheet As Boolean
Public OptionShowFullScreen As Boolean

Public JobPath As String * 255 'Must be 255 to interface properly with MODSIM.DLL
Public VGPath As String
Public FileReadPath As String
Public NameOfJob As String * 255
Public JobSavePath As String

Public WSTRING As String
