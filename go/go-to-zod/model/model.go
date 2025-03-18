package models

import (
	"time"

	"gorm.io/gorm"
)

const (
	// NvdType :
	NvdType = "NVD"
	// JvnType :
	JvnType = "JVN"
	// FortinetType :
	FortinetType = "Fortinet"
	// MitreType :
	MitreType = "Mitre"

	// NvdExactVersionMatch :
	NvdExactVersionMatch = "NvdExactVersionMatch"
	// NvdRoughVersionMatch :
	NvdRoughVersionMatch = "NvdRoughVersionMatch"
	// NvdVendorProductMatch :
	NvdVendorProductMatch = "NvdVendorProductMatch"
	// JvnVendorProductMatch :
	JvnVendorProductMatch = "JvnVendorProductMatch"
	// FortinetExactVersionMatch :
	FortinetExactVersionMatch = "FortinetExactVersionMatch"
	// FortinetRoughVersionMatch :
	FortinetRoughVersionMatch = "FortinetRoughVersionMatch"
	// FortinetVendorProductMatch :
	FortinetVendorProductMatch = "FortinetVendorProductMatch"
)

// LatestSchemaVersion manages the Schema version used in the latest go-cve-dictionary.
const LatestSchemaVersion = 3

// FetchMeta has meta information about fetched CVE data
type FetchMeta struct {
	gorm.Model        `json:"-"`
	GoCVEDictRevision string
	SchemaVersion     uint
	LastFetchedAt     time.Time
}

// OutDated checks whether last fetched feed is out dated
func (f FetchMeta) OutDated() bool {
	return f.SchemaVersion != LatestSchemaVersion
}

// CveDetail :
type CveDetail struct {
	CveID     string
	Nvds      []Nvd
	Jvns      []Jvn
	Fortinets []Fortinet
	Mitres    []Mitre
}

// HasNvd returns true if NVD contents
func (c CveDetail) HasNvd() bool {
	return len(c.Nvds) != 0
}

// HasJvn returns true if JVN contents
func (c CveDetail) HasJvn() bool {
	return len(c.Jvns) != 0
}

// HasFortinet returns true if Fortinet contents
func (c CveDetail) HasFortinet() bool {
	return len(c.Fortinets) != 0
}

// HasMitre returns true if Mitre contents
func (c CveDetail) HasMitre() bool {
	return len(c.Mitres) != 0
}

// CpeDetail :
type CpeDetail struct {
	Nvds     []NvdCpe
	Jvns     []JvnCpe
	Fortinet []FortinetCpe
}

// Components common to Nvd and Jvn

// Cvss2 has CVSS Version 2 info
type Cvss2 struct {
	VectorString          string `gorm:"type:varchar(255)"`
	AccessVector          string `gorm:"type:varchar(255)"`
	AccessComplexity      string `gorm:"type:varchar(255)"`
	Authentication        string `gorm:"type:varchar(255)"`
	ConfidentialityImpact string `gorm:"type:varchar(255)"`
	IntegrityImpact       string `gorm:"type:varchar(255)"`
	AvailabilityImpact    string `gorm:"type:varchar(255)"`
	BaseScore             float64
	Severity              string `gorm:"type:varchar(255)"`
}

// Cvss3 has CVSS Version 3 info
type Cvss3 struct {
	VectorString          string `gorm:"type:varchar(255)"`
	AttackVector          string `gorm:"type:varchar(255)"`
	AttackComplexity      string `gorm:"type:varchar(255)"`
	PrivilegesRequired    string `gorm:"type:varchar(255)"`
	UserInteraction       string `gorm:"type:varchar(255)"`
	Scope                 string `gorm:"type:varchar(255)"`
	ConfidentialityImpact string `gorm:"type:varchar(255)"`
	IntegrityImpact       string `gorm:"type:varchar(255)"`
	AvailabilityImpact    string `gorm:"type:varchar(255)"`
	BaseScore             float64
	BaseSeverity          string `gorm:"type:varchar(255)"`
	ExploitabilityScore   float64
	ImpactScore           float64
}

// Cvss40 has CVSS Version 4.0 info
type Cvss40 struct {
	VectorString          string  `gorm:"type:varchar(255)"`
	BaseScore             float64 `json:"baseScore"`
	BaseSeverity          string  `gorm:"type:varchar(255)"`
	ThreatScore           *float64
	ThreatSeverity        *string `gorm:"type:varchar(255)"`
	EnvironmentalScore    *float64
	EnvironmentalSeverity *string `gorm:"type:varchar(255)"`
}

// CpeBase has common args of Cpe and EnvCpe
type CpeBase struct {
	URI                   string `gorm:"index;type:varchar(255)"`
	FormattedString       string `gorm:"index;type:varchar(255)"`
	WellFormedName        string `gorm:"type:text"`
	CpeWFN                `gorm:"embedded"`
	VersionStartExcluding string `gorm:"type:varchar(255)"`
	VersionStartIncluding string `gorm:"type:varchar(255)"`
	VersionEndExcluding   string `gorm:"type:varchar(255)"`
	VersionEndIncluding   string `gorm:"type:varchar(255)"`
}

// CpeWFN has CPE Well Formed name information
type CpeWFN struct {
	Part            string `gorm:"index;type:varchar(255)"`
	Vendor          string `gorm:"index;type:varchar(255)"`
	Product         string `gorm:"index;type:varchar(255)"`
	Version         string `gorm:"type:varchar(255)"`
	Update          string `gorm:"type:varchar(255)"`
	Edition         string `gorm:"type:varchar(255)"`
	Language        string `gorm:"type:varchar(255)"`
	SoftwareEdition string `gorm:"type:varchar(255)"`
	TargetSW        string `gorm:"type:varchar(255)"`
	TargetHW        string `gorm:"type:varchar(255)"`
	Other           string `gorm:"type:varchar(255)"`
}

// Reference holds reference information about the CVE.
type Reference struct {
	Link   string `gorm:"type:text"`
	Source string `gorm:"type:varchar(255)"`
	Tags   string `gorm:"type:varchar(255)"`
	Name   string `gorm:"type:text"`
}

// Cert holds CERT alerts.
type Cert struct {
	Title string `gorm:"type:text"`
	Link  string `gorm:"type:text"`
}

// Nvd is a struct of NVD JSON
// https://scap.nist.gov/schema/nvd/feed/0.1/nvd_cve_feed_json_0.1_beta.schema
type Nvd struct {
	ID               int64  `json:"-"`
	CveID            string `gorm:"index:idx_nvds_cveid;type:varchar(255)"`
	Descriptions     []NvdDescription
	Cvss2            []NvdCvss2Extra
	Cvss3            []NvdCvss3
	Cvss40           []NvdCvss40
	Cwes             []NvdCwe
	Cpes             []NvdCpe
	References       []NvdReference
	Certs            []NvdCert
	PublishedDate    time.Time
	LastModifiedDate time.Time

	DetectionMethod string `gorm:"-"`
}

// NvdDescription has description of the CVE
type NvdDescription struct {
	ID    int64  `json:"-"`
	NvdID uint   `json:"-" gorm:"index:idx_nvd_descriptions_nvd_id"`
	Lang  string `gorm:"type:varchar(255)"`
	Value string `gorm:"type:text"`
}

// NvdCvss2Extra has Nvd extra CVSS V2 info
type NvdCvss2Extra struct {
	ID                      int64  `json:"-"`
	NvdID                   uint   `json:"-" gorm:"index:idx_nvd_cvss2_extra_nvd_id"`
	Source                  string `gorm:"type:text"`
	Type                    string `gorm:"type:varchar(255)"`
	Cvss2                   `gorm:"embedded"`
	ExploitabilityScore     float64
	ImpactScore             float64
	ObtainAllPrivilege      bool
	ObtainUserPrivilege     bool
	ObtainOtherPrivilege    bool
	UserInteractionRequired bool
}

// NvdCvss3 has Nvd CVSS3 info
type NvdCvss3 struct {
	ID     int64  `json:"-"`
	NvdID  uint   `json:"-" gorm:"index:idx_nvd_cvss3_nvd_id"`
	Source string `gorm:"type:text"`
	Type   string `gorm:"type:varchar(255)"`
	Cvss3  `gorm:"embedded"`
}

// NvdCvss40 has Nvd CVSS40 info
type NvdCvss40 struct {
	ID     int64  `json:"-"`
	NvdID  uint   `json:"-" gorm:"index:idx_nvd_cvss40_nvd_id"`
	Source string `gorm:"type:text"`
	Type   string `gorm:"type:varchar(255)"`
	Cvss40 `gorm:"embedded"`
}

// NvdCwe has CweID
type NvdCwe struct {
	ID     int64  `json:"-"`
	NvdID  uint   `json:"-" gorm:"index:idx_nvd_cwes_nvd_id"`
	Source string `gorm:"type:text"`
	Type   string `gorm:"type:varchar(255)"`
	CweID  string `gorm:"type:varchar(255)"`
}

// NvdCpe is Child model of Nvd.
// see https://www.ipa.go.jp/security/vuln/CPE.html
// In NVD,
// configurations>nodes>cpe>vulnerable: true
type NvdCpe struct {
	ID      int64 `json:"-"`
	NvdID   uint  `json:"-" gorm:"index:idx_nvd_cpes_nvd_id"`
	CpeBase `gorm:"embedded"`
	EnvCpes []NvdEnvCpe
}

// NvdEnvCpe is a Environmental CPE
// Only NVD has this information.
// configurations>nodes>cpe>vulnerable: false
type NvdEnvCpe struct {
	ID       int64 `json:"-"`
	NvdCpeID uint  `json:"-" gorm:"index:idx_nvd_env_cpes_nvd_cpe_id"`
	CpeBase  `gorm:"embedded"`
}

// NvdReference holds reference information about the CVE.
type NvdReference struct {
	ID        int64 `json:"-"`
	NvdID     uint  `json:"-" gorm:"index:idx_nvd_references_nvd_id"`
	Reference `gorm:"embedded"`
}

// NvdCert is Child model of Nvd.
type NvdCert struct {
	ID    int64 `json:"-"`
	NvdID uint  `json:"-" gorm:"index:idx_nvd_certs_nvd_id"`
	Cert  `gorm:"embedded"`
}

// Jvn is a model of JVN
type Jvn struct {
	ID               int64  `json:"-"`
	CveID            string `gorm:"index:idx_jvns_cveid;type:varchar(255)"`
	Title            string `gorm:"type:varchar(255)"`
	Summary          string `gorm:"type:text"`
	JvnLink          string `gorm:"type:varchar(255)"`
	JvnID            string `gorm:"type:varchar(255)"`
	Cvss2            JvnCvss2
	Cvss3            JvnCvss3
	Cpes             []JvnCpe
	References       []JvnReference
	Certs            []JvnCert
	PublishedDate    time.Time
	LastModifiedDate time.Time

	DetectionMethod string `gorm:"-"`
}

// JvnCvss2 has Jvn CVSS Version 2 info
type JvnCvss2 struct {
	ID    int64 `json:"-"`
	JvnID uint  `json:"-" gorm:"index:idx_jvn_cvss2_jvn_id"`
	Cvss2 `gorm:"embedded"`
}

// JvnCvss3 has JVN CVSS3 info
type JvnCvss3 struct {
	ID    int64 `json:"-"`
	JVNID uint  `json:"-" gorm:"index:idx_jvn_cvss3_jvn_id"`
	Cvss3 `gorm:"embedded"`
}

// JvnCpe is Child model of Jvn.
// see https://www.ipa.go.jp/security/vuln/CPE.html
type JvnCpe struct {
	ID      int64 `json:"-"`
	JvnID   uint  `json:"-" gorm:"index:idx_jvn_cpes_jvn_id"`
	CpeBase `gorm:"embedded"`
}

// JvnReference is Child model of Jvn.
type JvnReference struct {
	ID        int64 `json:"-"`
	JvnID     uint  `json:"-" gorm:"index:idx_jvn_references_jvn_id"`
	Reference `gorm:"embedded"`
}

// JvnCert is Child model of Jvn.
type JvnCert struct {
	ID    int64 `json:"-"`
	JvnID uint  `json:"-" gorm:"index:idx_jvn_certs_jvn_id"`
	Cert  `gorm:"embedded"`
}

// Fortinet is a model of Fortinet
type Fortinet struct {
	ID               int64  `json:"-"`
	AdvisoryID       string `gorm:"type:varchar(255)"`
	CveID            string `gorm:"index:idx_fortinets_cveid;type:varchar(255)"`
	Title            string `gorm:"type:varchar(255)"`
	Summary          string `gorm:"type:text"`
	Descriptions     string `gorm:"type:text"`
	Cvss3            FortinetCvss3
	Cwes             []FortinetCwe
	Cpes             []FortinetCpe
	References       []FortinetReference
	PublishedDate    time.Time
	LastModifiedDate time.Time
	AdvisoryURL      string `gorm:"type:text"`

	DetectionMethod string `gorm:"-"`
}

// FortinetCvss3 has Fortinet CVSS3 info
type FortinetCvss3 struct {
	ID         int64 `json:"-"`
	FortinetID uint  `json:"-" gorm:"index:idx_fortinet_cvss3_fortinet_id"`
	Cvss3      `gorm:"embedded"`
}

// FortinetCwe has CweID
type FortinetCwe struct {
	ID         int64  `json:"-"`
	FortinetID uint   `json:"-" gorm:"index:idx_fortinet_cwes_fortinet_id"`
	CweID      string `gorm:"type:varchar(255)"`
}

// FortinetCpe is Child model of Fortinet.
type FortinetCpe struct {
	ID         int64 `json:"-"`
	FortinetID uint  `json:"-" gorm:"index:idx_fortinet_cpes_fortinet_id"`
	CpeBase    `gorm:"embedded"`
}

// FortinetReference holds reference information about the CVE.
type FortinetReference struct {
	ID         int64 `json:"-"`
	FortinetID uint  `json:"-" gorm:"index:idx_fortinet_references_fortinet_id"`
	Reference  `gorm:"embedded"`
}

// Mitre : https://cveproject.github.io/cve-schema/schema/CVE_Record_Format.json
type Mitre struct {
	ID          int64  `json:"-"`
	DataType    string `gorm:"type:varchar(255)"`
	DataVersion string `gorm:"type:varchar(255)"`
	CVEMetadata MitreCVEMetadata
	Containers  []MitreContainer
}

// MitreCVEMetadata : #/definitions/cveMetadataPublished
type MitreCVEMetadata struct {
	ID                int64   `json:"-"`
	MitreID           uint    `json:"-" gorm:"index:idx_mitre_cve_metadata"`
	CVEID             string  `gorm:"index:idx_mitre_cve_metadata_cveid;type:varchar(255)"`
	AssignerOrgID     string  `gorm:"type:varchar(255)"`
	AssignerShortName *string `gorm:"type:varchar(32)"`
	RequesterUserID   *string `gorm:"type:varchar(255)"`
	Serial            *int
	State             string `gorm:"type:varchar(255)"`
	DatePublished     *time.Time
	DateUpdated       *time.Time
	DateReserved      *time.Time
	DateRejected      *time.Time
}

// MitreContainer : #/definitions/cnaPublishedContainer, #/definitions/adpContainer
type MitreContainer struct {
	ID               int64  `json:"-"`
	MitreID          uint   `json:"-" gorm:"index:idx_mitre_containers"`
	ContainerType    string `gorm:"type:varchar(255)"`
	ProviderMetadata MitreProviderMetadata
	Title            *string `gorm:"type:varchar(256)"`
	Descriptions     []MitreDescription
	Affected         []MitreProduct
	ProblemTypes     []MitreProblemType
	Impacts          []MitreImpact
	Metrics          []MitreMetric
	Workarounds      []MitreWorkaround
	Solutions        []MitreSolution
	Exploits         []MitreExploit
	Configurations   []MitreConfiguration
	References       []MitreReference
	Timeline         []MitreTimeline
	Credits          []MitreCredit
	Source           string `gorm:"type:text"`
	Tags             []MitreTag
	TaxonomyMappings []MitreTaxonomyMapping
	DateAssigned     *time.Time
	DatePublic       *time.Time
}

// MitreProviderMetadata : #/definitions/providerMetadata
type MitreProviderMetadata struct {
	ID               int64   `json:"-"`
	MitreContainerID uint    `json:"-" gorm:"index:idx_mitre_provider_metadata"`
	OrgID            string  `gorm:"type:varchar(255)"`
	ShortName        *string `gorm:"type:varchar(32)"`
	DateUpdated      *time.Time
}

// MitreDescription : #/definitions/description
type MitreDescription struct {
	ID               int64  `json:"-"`
	MitreContainerID uint   `json:"-" gorm:"index:idx_mitre_description"`
	Lang             string `gorm:"type:varchar(255)"`
	Value            string `gorm:"type:text"`
	SupportingMedia  []MitreDescriptionSupportingMedia
}

// MitreDescriptionSupportingMedia : #/definitions/description
type MitreDescriptionSupportingMedia struct {
	ID                 int64  `json:"-"`
	MitreDescriptionID uint   `json:"-" gorm:"index:idx_mitre_description_supporting_media"`
	Type               string `gorm:"type:varchar(256)"`
	Base64             *bool
	Value              string `gorm:"type:text"`
}

// MitreProduct : #/definitions/product
type MitreProduct struct {
	ID               int64   `json:"-"`
	MitreContainerID uint    `json:"-" gorm:"index:idx_mitre_product"`
	Vendor           *string `gorm:"type:text"`
	Product          *string `gorm:"type:text"`
	CollectionURL    *string `gorm:"type:text"`
	PackageName      *string `gorm:"type:text"`
	Cpes             []MitreProductCPE
	Modules          []MitreProductModule
	ProgramFiles     []MitreProductProgramFile
	ProgramRoutines  []MitreProductProgramRoutine
	Platforms        []MitreProductPlatform
	Repo             *string `gorm:"type:text"`
	DefaultStatus    *string
	Versions         []MitreProductVersion
}

// MitreProductCPE : #/definitions/product
type MitreProductCPE struct {
	ID             int64  `json:"-"`
	MitreProductID uint   `json:"-" gorm:"index:idx_mitre_product_cpe"`
	CPE            string `gorm:"type:text"`
}

// MitreProductModule : #/definitions/product
type MitreProductModule struct {
	ID             int64  `json:"-"`
	MitreProductID uint   `json:"-" gorm:"index:idx_mitre_product_module"`
	Module         string `gorm:"type:text"`
}

// MitreProductProgramFile : #/definitions/product
type MitreProductProgramFile struct {
	ID             int64  `json:"-"`
	MitreProductID uint   `json:"-" gorm:"index:idx_mitre_product_program_file"`
	ProgramFile    string `gorm:"type:text"`
}

// MitreProductProgramRoutine : #/definitions/product
type MitreProductProgramRoutine struct {
	ID             int64  `json:"-"`
	MitreProductID uint   `json:"-" gorm:"index:idx_mitre_product_program_routine"`
	Name           string `gorm:"type:text"`
}

// MitreProductPlatform : #/definitions/product
type MitreProductPlatform struct {
	ID             int64  `json:"-"`
	MitreProductID uint   `json:"-" gorm:"index:idx_mitre_product_platform"`
	Platform       string `gorm:"type:text"`
}

// MitreProductVersion : #/definitions/product
type MitreProductVersion struct {
	ID              int64   `json:"-"`
	MitreProductID  uint    `json:"-" gorm:"index:idx_mitre_product_version"`
	Status          string  `gorm:"type:varchar(255)"`
	VersionType     *string `gorm:"type:varchar(128)"`
	Version         string  `gorm:"type:text"`
	LessThan        *string `gorm:"type:text"`
	LessThanOrEqual *string `gorm:"type:text"`
	Changes         []MitreProductVersionChange
}

// MitreProductVersionChange : #/definitions/product
type MitreProductVersionChange struct {
	ID                    int64  `json:"-"`
	MitreProductVersionID uint   `json:"-" gorm:"index:idx_mitre_product_version_change"`
	At                    string `gorm:"type:text"`
	Status                string `gorm:"type:varchar(255)"`
}

// MitreProblemType : #/definitions/problemTypes
type MitreProblemType struct {
	ID               int64 `json:"-"`
	MitreContainerID uint  `json:"-" gorm:"index:idx_mitre_problem_type"`
	Descriptions     []MitreProblemTypeDescription
}

// MitreProblemTypeDescription : #/definitions/problemTypes
type MitreProblemTypeDescription struct {
	ID                 int64   `json:"-"`
	MitreProblemTypeID uint    `json:"-" gorm:"index:idx_mitre_problem_type_description"`
	Type               *string `gorm:"type:varchar(255)"`
	Lang               string  `gorm:"type:varchar(128)"`
	Description        string  `gorm:"type:text"`
	CweID              *string `gorm:"type:varchar(9)"`
	References         []MitreProblemTypeDescriptionReference
}

// MitreProblemTypeDescriptionReference : #/definitions/references
type MitreProblemTypeDescriptionReference struct {
	ID                            int64 `json:"-"`
	MitreProblemTypeDescriptionID uint  `json:"-" gorm:"index:idx_mitre_problem_type_description_reference"`
	Reference                     `gorm:"embedded"`
}

// MitreImpact : #/definitions/impacts
type MitreImpact struct {
	ID               int64 `json:"-"`
	MitreContainerID uint  `json:"-" gorm:"index:idx_mitre_impact"`
	Descriptions     []MitreImpactDescription
	CapecID          *string `gorm:"type:varchar(11)"`
}

// MitreImpactDescription : #/definitions/description
type MitreImpactDescription struct {
	ID              int64  `json:"-"`
	MitreImpactID   uint   `json:"-" gorm:"index:idx_mitre_impact_description"`
	Lang            string `gorm:"type:varchar(255)"`
	Value           string `gorm:"type:text"`
	SupportingMedia []MitreImpactDescriptionSupportingMedia
}

// MitreImpactDescriptionSupportingMedia : #/definitions/description
type MitreImpactDescriptionSupportingMedia struct {
	ID                       int64  `json:"-"`
	MitreImpactDescriptionID uint   `json:"-" gorm:"index:idx_mitre_impact_description_supporting_media"`
	Type                     string `gorm:"type:varchar(256)"`
	Base64                   *bool
	Value                    string `gorm:"type:text"`
}

// MitreMetric : #/definitions/metrics
type MitreMetric struct {
	ID               int64  `json:"-"`
	MitreContainerID uint   `json:"-" gorm:"index:idx_mitre_metric"`
	Format           string `gorm:"type:varchar(64)"`
	Scenarios        []MitreMetricScenario
	CVSSv2           *MitreMetricCVSS2
	CVSSv30          *MitreMetricCVSS30
	CVSSv31          *MitreMetricCVSS31
	CVSSv40          *MitreMetricCVSS40
	SSVC             *MitreMetricSSVC
	KEV              *MitreMetricKEV
	Other            *MitreMetricOther
}

// MitreMetricScenario : #/definitions/metrics
type MitreMetricScenario struct {
	ID            int64  `json:"-"`
	MitreMetricID uint   `json:"-" gorm:"index:idx_mitre_metric_scenario"`
	Lang          string `gorm:"type:varchar(255)"`
	Value         string `gorm:"type:text"`
}

// MitreMetricCVSS2 : https://www.first.org/cvss/cvss-v2.0.json?20170531
type MitreMetricCVSS2 struct {
	ID            int64 `json:"-"`
	MitreMetricID uint  `json:"-" gorm:"index:idx_mitre_metric_cvss2"`
	Cvss2         `gorm:"embedded"`
}

// MitreMetricCVSS30 : https://www.first.org/cvss/cvss-v3.0.json?20170531
type MitreMetricCVSS30 struct {
	ID            int64 `json:"-"`
	MitreMetricID uint  `json:"-" gorm:"index:idx_mitre_metric_cvss30"`
	Cvss3         `gorm:"embedded"`
}

// MitreMetricCVSS31 : https://www.first.org/cvss/cvss-v3.1.json?20210501
type MitreMetricCVSS31 struct {
	ID            int64 `json:"-"`
	MitreMetricID uint  `json:"-" gorm:"index:idx_mitre_metric_cvss31"`
	Cvss3         `gorm:"embedded"`
}

// MitreMetricCVSS40 : https://www.first.org/cvss/cvss-v4.0.json?20231011
type MitreMetricCVSS40 struct {
	ID            int64 `json:"-"`
	MitreMetricID uint  `json:"-" gorm:"index:idx_mitre_metric_cvss40"`
	Cvss40        `gorm:"embedded"`
}

// MitreMetricSSVC :
type MitreMetricSSVC struct {
	ID              int64  `json:"-"`
	MitreMetricID   uint   `json:"-" gorm:"index:idx_mitre_metric_ssvc"`
	Role            string `gorm:"type:varchar(255)"`
	Version         string `gorm:"type:varchar(255)"`
	Timestamp       time.Time
	Exploitation    *string `gorm:"type:varchar(255)"`
	Automatable     *string `gorm:"type:varchar(255)"`
	TechnicalImpact *string `gorm:"type:varchar(255)"`
}

// MitreMetricKEV : https://github.com/cisagov/vulnrichment/blob/3f9d69632037fae3b7abdf47fc848c287702ffaa/assets/kev_metrics_schema-1.0.json
type MitreMetricKEV struct {
	ID            int64 `json:"-"`
	MitreMetricID uint  `json:"-" gorm:"index:idx_mitre_metric_kev"`
	DateAdded     time.Time
	Reference     string `gorm:"type:varchar(255)"`
}

// MitreMetricOther : https://github.com/CVEProject/cve-schema/blob/30f59c7de92fbc77bddade302601cb500c66f718/schema/CVE_Record_Format.json#L901-L923
type MitreMetricOther struct {
	ID            int64  `json:"-"`
	MitreMetricID uint   `json:"-" gorm:"index:idx_mitre_metric_other"`
	Type          string `gorm:"type:varchar(128)"`
	Content       string `gorm:"type:text"`
}

// MitreWorkaround : #/definitions/workarounds
type MitreWorkaround struct {
	ID               int64  `json:"-"`
	MitreContainerID uint   `json:"-" gorm:"index:idx_mitre_workaround"`
	Lang             string `gorm:"type:varchar(255)"`
	Value            string `gorm:"type:text"`
	SupportingMedia  []MitreWorkaroundSupportingMedia
}

// MitreWorkaroundSupportingMedia : #/definitions/description
type MitreWorkaroundSupportingMedia struct {
	ID                int64  `json:"-"`
	MitreWorkaroundID uint   `json:"-" gorm:"index:idx_mitre_workaround_supporting_media"`
	Type              string `gorm:"type:varchar(256)"`
	Base64            *bool
	Value             string `gorm:"type:text"`
}

// MitreSolution : #/definitions/solutions
type MitreSolution struct {
	ID               int64  `json:"-"`
	MitreContainerID uint   `json:"-" gorm:"index:idx_mitre_solution"`
	Lang             string `gorm:"type:varchar(255)"`
	Value            string `gorm:"type:text"`
	SupportingMedia  []MitreSolutionSupportingMedia
}

// MitreSolutionSupportingMedia : #/definitions/description
type MitreSolutionSupportingMedia struct {
	ID              int64  `json:"-"`
	MitreSolutionID uint   `json:"-" gorm:"index:idx_mitre_solution_supporting_media"`
	Type            string `gorm:"type:varchar(256)"`
	Base64          *bool
	Value           string `gorm:"type:text"`
}

// MitreExploit : #/definitions/exploits
type MitreExploit struct {
	ID               int64  `json:"-"`
	MitreContainerID uint   `json:"-" gorm:"index:idx_mitre_exploit"`
	Lang             string `gorm:"type:varchar(255)"`
	Value            string `gorm:"type:text"`
	SupportingMedia  []MitreExploitSupportingMedia
}

// MitreExploitSupportingMedia : #/definitions/description
type MitreExploitSupportingMedia struct {
	ID             int64  `json:"-"`
	MitreExploitID uint   `json:"-" gorm:"index:idx_mitre_exploit_supporting_media"`
	Type           string `gorm:"type:varchar(256)"`
	Base64         *bool
	Value          string `gorm:"type:text"`
}

// MitreConfiguration : #/definitions/configurations
type MitreConfiguration struct {
	ID               int64  `json:"-"`
	MitreContainerID uint   `json:"-" gorm:"index:idx_mitre_configuration"`
	Lang             string `gorm:"type:varchar(255)"`
	Value            string `gorm:"type:text"`
	SupportingMedia  []MitreConfigurationSupportingMedia
}

// MitreConfigurationSupportingMedia : #/definitions/description
type MitreConfigurationSupportingMedia struct {
	ID                   int64  `json:"-"`
	MitreConfigurationID uint   `json:"-" gorm:"index:idx_mitre_configuration_supporting_media"`
	Type                 string `gorm:"type:varchar(256)"`
	Base64               *bool
	Value                string `gorm:"type:text"`
}

// MitreReference : #/definitions/references
type MitreReference struct {
	ID               int64 `json:"-"`
	MitreContainerID uint  `json:"-" gorm:"index:idx_mitre_reference"`
	Reference        `gorm:"embedded"`
}

// MitreTimeline : #/definitions/timeline
type MitreTimeline struct {
	ID               int64 `json:"-"`
	MitreContainerID uint  `json:"-" gorm:"index:idx_mitre_timeline"`
	Time             time.Time
	Lang             string `gorm:"type:varchar(255)"`
	Value            string `gorm:"type:text"`
}

// MitreCredit : #/definitions/credits
type MitreCredit struct {
	ID               int64   `json:"-"`
	MitreContainerID uint    `json:"-" gorm:"index:idx_mitre_credit"`
	Type             *string `gorm:"type:varchar(255)"`
	Lang             string  `gorm:"type:varchar(255)"`
	User             *string `gorm:"type:varchar(255)"`
	Value            string  `gorm:"type:text"`
}

// MitreTag : #/definitions/cnaTags, #/definitions/adpTags
type MitreTag struct {
	ID               int64  `json:"-"`
	MitreContainerID uint   `json:"-" gorm:"index:idx_mitre_tag"`
	Tag              string `gorm:"type:varchar(255)"`
}

// MitreTaxonomyMapping : #/definitions/taxonomyMappings
type MitreTaxonomyMapping struct {
	ID                int64   `json:"-"`
	MitreContainerID  uint    `json:"-" gorm:"index:idx_mitre_taxonomy_mapping"`
	TaxonomyVersion   *string `gorm:"type:varchar(128)"`
	TaxonomyName      string  `gorm:"type:varchar(128)"`
	TaxonomyRelations []MitreTaxonomyRelation
}

// MitreTaxonomyRelation : #/definitions/taxonomyMappings
type MitreTaxonomyRelation struct {
	ID                     int64  `json:"-"`
	MitreTaxonomyMappingID uint   `json:"-" gorm:"index:idx_mitre_taxonomy_relation"`
	TaxonomyID             string `gorm:"type:text"`
	RelationshipName       string `gorm:"type:varchar(128)"`
	RelationshipValue      string `gorm:"type:text"`
}