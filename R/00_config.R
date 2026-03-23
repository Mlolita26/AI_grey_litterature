##############################################################################
# 00_config.R — Configuration for Grey Literature Downloader
# Keywords, paths, and shared settings for all scrapers
##############################################################################

# ── Paths ──────────────────────────────────────────────────────────────────
# Set PROJECT_ROOT to wherever you cloned/placed this project.
# Works with: Rscript, source(), and interactive R sessions.
#
# OPTION: If auto-detection fails, hard-code your path here:
# PROJECT_ROOT <- "/path/to/grey_lit_downloader"

# Helper: given the directory that contains 00_config.R, determine PROJECT_ROOT.
# Rule: if a .Rproj file lives in that directory, it IS the project root.
#       Otherwise fall back to one level up (legacy behaviour).
.resolve_project_root <- function(script_dir) {
  script_dir <- normalizePath(script_dir, mustWork = FALSE)
  rproj <- list.files(script_dir, pattern = "\\.Rproj$", full.names = FALSE)
  if (length(rproj) > 0) {
    script_dir          # project root == scripts directory
  } else {
    normalizePath(file.path(script_dir, ".."), mustWork = FALSE)
  }
}

PROJECT_ROOT <- tryCatch({
  # Method 1: works when run via source()
  script_path <- sys.frame(1)$ofile
  if (!is.null(script_path) && nzchar(script_path)) {
    .resolve_project_root(dirname(script_path))
  } else {
    stop("no ofile")
  }
}, error = function(e) {
  tryCatch({
    # Method 2: works when run via Rscript
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg) > 0) {
      script_path <- sub("^--file=", "", file_arg[1])
      .resolve_project_root(dirname(script_path))
    } else {
      stop("no --file arg")
    }
  }, error = function(e2) {
    # Method 3: fall back to working directory
    # If a .Rproj file is present in the working directory, use it as root.
    wd <- getwd()
    rproj_in_wd <- length(list.files(wd, pattern = "\\.Rproj$")) > 0
    if (rproj_in_wd) {
      wd
    } else if (file.exists(file.path(wd, "R", "00_config.R"))) {
      wd
    } else if (file.exists(file.path(wd, "00_config.R"))) {
      normalizePath(file.path(wd, ".."), mustWork = FALSE)
    } else {
      wd
    }
  })
})

PATHS <- list(
  root       = PROJECT_ROOT,
  r_dir      = file.path(PROJECT_ROOT, "R"),
  scrapers   = file.path(PROJECT_ROOT, "R", "scrapers"),
  data       = file.path(PROJECT_ROOT, "data"),
  downloads  = file.path(PROJECT_ROOT, "downloads"),
  reports    = file.path(PROJECT_ROOT, "reports"),
  log_file   = file.path(PROJECT_ROOT, "data", "download_log.csv")
)

# Create directories if they don't exist
for (p in PATHS) {
  if (!grepl("\\.", basename(p))) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
  }
}

# ── HTTP Settings ──────────────────────────────────────────────────────────
HTTP_CONFIG <- list(
  user_agent    = "GreyLitReview/1.0 (Academic research; climate adaptation Africa)",
  delay_min     = 1.0,

  delay_max     = 2.5,
  max_retries   = 3,
  timeout_sec   = 60,
  min_pdf_bytes = 10240  # 10KB minimum for valid PDF
)

# ── African Countries ──────────────────────────────────────────────────────
# All 54 AU member states + common English/French/Portuguese variants

AFRICA_COUNTRIES_EN <- c(
  "Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi",
  "Cabo Verde", "Cape Verde", "Cameroon", "Central African Republic",
  "Chad", "Comoros", "Congo", "Democratic Republic of Congo", "DRC",
  "Republic of Congo", "Cote d'Ivoire", "Ivory Coast", "Djibouti",
  "Egypt", "Equatorial Guinea", "Eritrea", "Eswatini", "Swaziland",

  "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau",
  "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi",
  "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia",
  "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe",
  "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa",
  "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda",
  "Zambia", "Zimbabwe"
)

AFRICA_COUNTRIES_FR <- c(
  "Algérie", "Bénin", "Burkina Faso", "Burundi", "Cameroun",

  "Cap-Vert", "Centrafrique", "République centrafricaine", "Comores",
  "République démocratique du Congo", "République du Congo",
  "Côte d'Ivoire", "Djibouti", "Égypte", "Érythrée",
  "Éthiopie", "Gabon", "Gambie", "Guinée", "Guinée équatoriale",
  "Guinée-Bissau", "Lesotho", "Libéria", "Libye", "Madagascar",
  "Malawi", "Maurice", "Mauritanie", "Maroc", "Mozambique",
  "Namibie", "Nigéria", "Ouganda", "Sénégal", "Somalie",
  "Soudan", "Soudan du Sud", "Tanzanie", "Tchad", "Tunisie",
  "Zambie", "Zimbabwe", "Afrique du Sud", "Togo", "Mali", "Niger",
  "Rwanda", "Seychelles", "Sierra Leone", "Sao Tomé-et-Príncipe",
  "Eswatini"
)

AFRICA_COUNTRIES_PT <- c(
  "Angola", "Cabo Verde", "Guiné-Bissau", "Guiné Equatorial",
  "Moçambique", "São Tomé e Príncipe", "África do Sul",
  "Argélia", "Benim", "Camarões", "Chade", "Comores",
  "República Democrática do Congo", "República do Congo",
  "Costa do Marfim", "Egito", "Eritreia", "Etiópia",
  "Gabão", "Gâmbia", "Gana", "Guiné", "Lesoto", "Libéria",
  "Líbia", "Madagáscar", "Maláui", "Marrocos", "Maurícia",
  "Mauritânia", "Namíbia", "Nigéria", "Quénia", "Ruanda",
  "Seicheles", "Serra Leoa", "Somália", "Sudão", "Sudão do Sul",
  "Tanzânia", "Tunísia", "Uganda", "Zâmbia", "Zimbábue",
  "Senegal", "Togo", "Mali", "Níger", "Botsuana", "Suazilândia"
)

# Regional terms
AFRICA_REGIONS <- c(
  # English
  "Africa", "African", "Sub-Saharan Africa", "sub-Saharan",
  "West Africa", "East Africa", "Southern Africa", "Central Africa",
  "North Africa", "Horn of Africa", "Sahel", "Great Lakes",
  "ECOWAS", "SADC", "COMESA", "EAC", "IGAD", "CEMAC", "UEMOA",
  "WAEMU", "ECCAS",
  # French
  "Afrique", "Afrique subsaharienne", "Afrique de l'Ouest",

  "Afrique de l'Est", "Afrique australe", "Afrique centrale",
  "Afrique du Nord", "Corne de l'Afrique",
  # Portuguese
  "África", "África Subsaariana", "África Ocidental",
  "África Oriental", "África Austral", "África Central"
)

# Combined: all Africa terms for matching
AFRICA_ALL <- unique(tolower(c(
  AFRICA_COUNTRIES_EN, AFRICA_COUNTRIES_FR, AFRICA_COUNTRIES_PT,
  AFRICA_REGIONS
)))

# ── Agriculture Sector Keywords ────────────────────────────────────────────
AGRICULTURE_KEYWORDS <- list(
  en = c(
    "agriculture", "agricultural", "farming", "farm", "crop", "crops",
    "livestock", "pastoral", "pastoralist", "fisheries", "fishery",
    "aquaculture", "agroforestry", "food security", "food system",
    "food production", "value chain", "agribusiness", "smallholder",
    "irrigation", "seed", "fertilizer", "soil", "land use",
    "cereal", "maize", "rice", "wheat", "sorghum", "millet",
    "cassava", "yam", "cocoa", "coffee", "tea", "cotton",
    "horticulture", "vegetable", "fruit", "dairy", "poultry",
    "cattle", "goat", "sheep", "camel", "rural development",
    "rural livelihood", "food aid", "nutrition", "hunger",
    "agro-pastoral", "agropastoral", "dryland", "rangeland",
    "extension service", "farmer field school"
  ),
  fr = c(
    "agriculture", "agricole", "élevage", "culture", "récolte",
    "pêche", "aquaculture", "agroforesterie", "sécurité alimentaire",
    "système alimentaire", "chaîne de valeur", "petit exploitant",
    "irrigation", "semence", "engrais", "sol", "utilisation des terres",
    "céréale", "maïs", "riz", "blé", "sorgho", "mil", "manioc",
    "igname", "cacao", "café", "thé", "coton", "horticulture",
    "légume", "fruit", "laitier", "volaille", "bétail", "bovin",
    "caprin", "ovin", "chameau", "développement rural",
    "pastoralisme", "agropastoralisme", "parcours"
  ),
  pt = c(
    "agricultura", "agrícola", "pecuária", "cultivo", "colheita",
    "pesca", "aquicultura", "agrossilvicultura", "segurança alimentar",
    "sistema alimentar", "cadeia de valor", "pequeno agricultor",
    "irrigação", "semente", "fertilizante", "solo", "uso da terra",
    "cereal", "milho", "arroz", "trigo", "sorgo", "milheto",
    "mandioca", "inhame", "cacau", "café", "chá", "algodão",
    "horticultura", "legume", "fruta", "laticínio", "avicultura",
    "gado", "cabra", "ovelha", "desenvolvimento rural",
    "pastoralismo", "agropastoralismo"
  )
)

AGRICULTURE_ALL <- unique(tolower(unlist(AGRICULTURE_KEYWORDS)))

# ── Climate Adaptation Keywords ────────────────────────────────────────────
ADAPTATION_KEYWORDS <- list(
  en = c(
    "adaptation", "climate adaptation", "climate change adaptation",
    "climate resilience", "resilience", "climate-smart",
    "climate smart agriculture", "CSA", "drought", "flood",
    "rainfall variability", "water scarcity", "desertification",
    "land degradation", "soil erosion", "climate risk",
    "climate vulnerability", "early warning", "weather index",
    "crop insurance", "index insurance", "disaster risk reduction",
    "DRR", "climate information", "climate services",
    "adaptive capacity", "vulnerability reduction",
    "water management", "water harvesting", "conservation agriculture",
    "climate-proofing", "heat stress", "sea level rise",
    "salinization", "pest", "disease outbreak",
    "food crisis", "famine", "El Nino", "La Nina",
    "LDCF", "SCCF", "NAP", "NAPA", "NDC",
    "nationally determined contribution"
  ),
  fr = c(
    "adaptation", "adaptation au changement climatique",
    "résilience climatique", "résilience",
    "agriculture intelligente face au climat",
    "sécheresse", "inondation", "variabilité des précipitations",
    "pénurie d'eau", "désertification", "dégradation des terres",
    "érosion des sols", "risque climatique",
    "vulnérabilité climatique", "alerte précoce",
    "assurance indicielle", "réduction des risques de catastrophe",
    "services climatiques", "capacité d'adaptation",
    "gestion de l'eau", "collecte des eaux", "agriculture de conservation",
    "stress thermique", "élévation du niveau de la mer",
    "salinisation", "ravageur", "crise alimentaire", "famine"
  ),
  pt = c(
    "adaptação", "adaptação às mudanças climáticas",
    "resiliência climática", "resiliência",
    "agricultura climaticamente inteligente",
    "seca", "inundação", "variabilidade das chuvas",
    "escassez de água", "desertificação", "degradação da terra",
    "erosão do solo", "risco climático",
    "vulnerabilidade climática", "alerta precoce",
    "seguro indexado", "redução do risco de desastres",
    "serviços climáticos", "capacidade adaptativa",
    "gestão da água", "captação de água", "agricultura de conservação",
    "estresse térmico", "elevação do nível do mar",
    "salinização", "praga", "crise alimentar", "fome"
  )
)

ADAPTATION_ALL <- unique(tolower(unlist(ADAPTATION_KEYWORDS)))

# ── Document Type Keywords ─────────────────────────────────────────────────
DOC_TYPE_KEYWORDS <- list(
  en = c(
    "implementation completion", "ICR", "completion report",
    "terminal evaluation", "final evaluation", "mid-term evaluation",
    "midterm evaluation", "mid-term review", "midterm review",
    "project completion report", "PCR",
    "project performance assessment", "PPAR",
    "impact evaluation", "impact assessment",
    "country program evaluation", "portfolio evaluation",
    "thematic evaluation", "annual performance report", "APR",
    "implementation status", "ISR", "supervision report",
    "progress report", "monitoring report",
    "project appraisal document", "PAD",
    "results report", "outcome evaluation",
    "lessons learned", "evaluation report",
    "ex-post evaluation", "effectiveness review"
  ),
  fr = c(
    "rapport d'achèvement", "évaluation terminale", "évaluation finale",
    "évaluation à mi-parcours", "revue à mi-parcours",
    "rapport d'achèvement de projet",
    "évaluation d'impact", "évaluation de l'impact",
    "évaluation de programme pays", "évaluation de portefeuille",
    "évaluation thématique", "rapport de performance annuel",
    "rapport d'avancement", "rapport de suivi",
    "document d'évaluation de projet",
    "rapport de résultats", "évaluation des résultats",
    "leçons apprises", "rapport d'évaluation"
  ),
  pt = c(
    "relatório de conclusão", "avaliação terminal", "avaliação final",
    "avaliação intercalar", "revisão intercalar",
    "relatório de conclusão do projeto",
    "avaliação de impacto", "avaliação do impacto",
    "avaliação do programa do país", "avaliação de carteira",
    "avaliação temática", "relatório de desempenho anual",
    "relatório de progresso", "relatório de monitoramento",
    "documento de avaliação do projeto",
    "relatório de resultados", "avaliação dos resultados",
    "lições aprendidas", "relatório de avaliação"
  )
)

DOC_TYPE_ALL <- unique(tolower(unlist(DOC_TYPE_KEYWORDS)))

# ── World Bank specific: African country codes (ISO2) ──────────────────────
# Used for the WB API count_exact or country filters
WB_AFRICA_CODES <- c(
  "DZ", "AO", "BJ", "BW", "BF", "BI", "CV", "CM", "CF", "TD",
  "KM", "CD", "CG", "CI", "DJ", "EG", "GQ", "ER", "SZ", "ET",
  "GA", "GM", "GH", "GN", "GW", "KE", "LS", "LR", "LY", "MG",
  "MW", "ML", "MR", "MU", "MA", "MZ", "NA", "NE", "NG", "RW",
  "ST", "SN", "SC", "SL", "SO", "ZA", "SS", "SD", "TZ", "TG",
  "TN", "UG", "ZM", "ZW"
)

# Mapping ISO2 → country name (for WB API which uses full names)
WB_AFRICA_NAMES <- c(
  DZ = "Algeria", AO = "Angola", BJ = "Benin", BW = "Botswana",
  BF = "Burkina Faso", BI = "Burundi", CV = "Cabo Verde",
  CM = "Cameroon", CF = "Central African Republic", TD = "Chad",
  KM = "Comoros", CD = "Congo, Democratic Republic of",
  CG = "Congo, Republic of", CI = "Cote d'Ivoire", DJ = "Djibouti",
  EG = "Egypt, Arab Republic of", GQ = "Equatorial Guinea",
  ER = "Eritrea", SZ = "Eswatini", ET = "Ethiopia",
  GA = "Gabon", GM = "Gambia, The", GH = "Ghana", GN = "Guinea",
  GW = "Guinea-Bissau", KE = "Kenya", LS = "Lesotho",
  LR = "Liberia", LY = "Libya", MG = "Madagascar", MW = "Malawi",
  ML = "Mali", MR = "Mauritania", MU = "Mauritius", MA = "Morocco",
  MZ = "Mozambique", "NA" = "Namibia", NE = "Niger", NG = "Nigeria",
  RW = "Rwanda", ST = "Sao Tome and Principe", SN = "Senegal",
  SC = "Seychelles", SL = "Sierra Leone", SO = "Somalia",
  ZA = "South Africa", SS = "South Sudan", SD = "Sudan",
  TZ = "Tanzania", TG = "Togo", TN = "Tunisia", UG = "Uganda",
  ZM = "Zambia", ZW = "Zimbabwe"
)

cat("✓ Config loaded. PROJECT_ROOT:", PROJECT_ROOT, "\n")
