devtools::load_all()
library(tibble)
data_config(messages = TRUE)
data_attach("flt3.aml.zfish.datapkg")
data_attach(c("flt3.aml.zfish.datapkg", "hgf.bcor.datapkg"))
data_catalog()
seq_qc
data_detach()

data_get("seq_qc", "flt3.aml.zfish.datapkg") |> dplyr::filter(sample == "u_neg_1")
