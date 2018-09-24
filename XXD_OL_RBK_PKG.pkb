CREATE OR REPLACE PACKAGE BODY APPS.XXD_OL_RBK_PKG
AS
   /* $DLLHeader~RelOL~1.43$ */
   /*******************************************************************************
  * $Header$
  * Program Name : XXD_OL_RBK_PKG.pkb
  * Language     : PL/SQL
  * Description  : This PL/SQL will be used for reclassifying/p_kle_iding the DFL contracts to OL contracts
   History      :
  * WHO             WHAT                                    WHEN
  * --------------  --------------------------------------- ---------------
  * Sherin       Initial Version.                        06-May-2016
  * Priya                                                06-Sep-2016
  * Sherin                                               14-Sep-2016
  * Sherin      Deprn Expense Fix - Ver1.35              21-Oct-2016
  * Sherin      Added condn to prevent duplicate lines   28-Oct-2016
  ******************************************************************************/
   gn_org_id                          NUMBER := fnd_global.org_id;
   gd_profile_val                     VARCHAR2 (1)
      := NVL (Fnd_Profile.VALUE ('DLL_LEASE_RECLASSIFICATION_ORG'), 'N');
   gd_pdt_name                        VARCHAR2 (30)
      := Fnd_Profile.VALUE ('DLL_LEASE_RECLASSIFICATION_PDT');
   gn_api_version            CONSTANT NUMBER := 1.0;

   -- Added for fixing the syndicated contracts issue - deprn expense account
   gn_sob_id                          NUMBER := fnd_profile.VALUE ('gl_set_of_bks_id');
   gn_user_id                         fnd_user.user_id%TYPE := fnd_global.user_id;

   --Added for the non-accrual changes

   g_contract_number_token   CONSTANT VARCHAR2 (200) := 'CONTRACT_NUMBER';
   g_app_name                CONSTANT VARCHAR2 (3) := Okl_Api.g_app_name;

   -- For the already reclassified/pending rebook/quote contracts
   ln_reclas_cnt                      NUMBER;
   ln_rbk_cnt                         NUMBER;
   ln_qte_cnt                         NUMBER;

   PROCEDURE create_fa_adj_cta_asst_prc (
      p_contract_id     IN            okc_k_headers_b.ID%TYPE,
      x_return_status      OUT NOCOPY VARCHAR2,
      x_msg_data           OUT NOCOPY VARCHAR2)
   IS
      CURSOR get_cta_asset_c (
         p_contract_id NUMBER)
      IS
         SELECT okl.name asset_name, okh.id contract_id, okl.id kle_id
           FROM okc_k_headers_b okh, okc_k_lines_v okl, okc_line_styles_b ols
          WHERE     1 = 1
                AND okl.sts_code = 'BOOKED'
                AND okh.id = p_contract_id
                AND okl.dnz_chr_id = okh.id
                AND okl.lse_id = ols.id
                AND ols.lty_code = 'FREE_FORM1'
                AND okl.orig_system_id1 IS NOT NULL
                --Start of modification by Priya on 17-Jan-2017
                AND TRUNC (okh.start_date) <> TRUNC (okl.start_date)
                --End of modification by Priya on 17-Jan-2017 
                AND EXISTS
                       (SELECT 1
                          FROM okl_trx_contracts otc
                         WHERE     okl.dnz_chr_id = otc.khr_id
                               AND okh.id = otc.khr_id
                               AND tsu_code = 'PROCESSED'
                               AND rbr_code LIKE 'CTA%');

      CURSOR get_rbk_khr_id_c (
         p_contract_id NUMBER)
      IS
         SELECT otc.khr_id_new
           FROM okl_trx_contracts otc, okc_k_headers_b okh
          WHERE     tsu_code = 'PROCESSED'
                AND rbr_code LIKE 'RECLASS_OPL'
                AND okh.id = otc.khr_id
                AND okh.id = p_contract_id;

      CURSOR get_asset_cost (p_contract_id2 NUMBER, p_asset_name VARCHAR2)
      IS
         SELECT depreciation_cost
           FROM okl_txl_assets_b
          WHERE dnz_khr_id = p_contract_id2 AND asset_number = p_asset_name;


      CURSOR get_asset_id (
         p_asset_name VARCHAR2)
      IS
         SELECT fab.asset_id, fbc.book_type_code
           FROM fa_additions_b fab, fa_books fb, fa_book_controls fbc
          WHERE     fab.asset_number = p_asset_name
                AND fb.asset_id = fab.asset_id
                AND fbc.book_type_code = fb.book_type_code
                AND fbc.book_class = 'CORPORATE'
                AND ROWNUM = 1;

      CURSOR get_salvage_value_c (p_kle_id OKL_K_LINES_V.ID%TYPE)
      IS
         SELECT residual_value
           FROM OKL_K_LINES a
          WHERE id = p_kle_id;

      ln_asset_cost                 NUMBER;
      ln_rbk_khr_id                 NUMBER;
      ln_asset_id                   NUMBER;
      lc_book_type_code             fa_book_controls.book_type_code%TYPE;
      l_trans_rec                   fa_api_types.trans_rec_type;
      l_asset_hdr_rec               fa_api_types.asset_hdr_rec_type;
      l_asset_fin_rec_adj           fa_api_types.asset_fin_rec_type;
      l_asset_fin_rec_new           fa_api_types.asset_fin_rec_type;
      l_asset_fin_mrc_tbl_new       fa_api_types.asset_fin_tbl_type;
      l_inv_trans_rec               fa_api_types.inv_trans_rec_type;
      l_inv_tbl                     fa_api_types.inv_tbl_type;
      l_inv_rate_tbl                fa_api_types.inv_rate_tbl_type;
      l_asset_deprn_rec_adj         fa_api_types.asset_deprn_rec_type;
      l_asset_deprn_rec_new         fa_api_types.asset_deprn_rec_type;
      l_asset_deprn_mrc_tbl_new     fa_api_types.asset_deprn_tbl_type;
      l_inv_rec                     fa_api_types.inv_rec_type;
      l_group_reclass_options_rec   fa_api_types.group_reclass_options_rec_type;
      l_return_status2              VARCHAR2 (1);
      l_mesg_count                  NUMBER := 0;
      l_mesg_len                    NUMBER;
      l_mesg                        VARCHAR2 (4000);
      lc_return_status2             VARCHAR2 (1);
      ln_salvage_value              NUMBER;
   BEGIN
      x_return_status := 'S';

      FOR lcu_cta_asset_c IN get_cta_asset_c (p_contract_id)
      LOOP
         ln_asset_id := NULL;
         ln_rbk_khr_id := NULL;
         ln_asset_id := NULL;
         lc_book_type_code := NULL;
         ln_salvage_value := NULL;

         OPEN get_rbk_khr_id_c (lcu_cta_asset_c.contract_id);

         FETCH get_rbk_khr_id_c INTO ln_rbk_khr_id;

         CLOSE get_rbk_khr_id_c;

         OPEN get_asset_cost (ln_rbk_khr_id, lcu_cta_asset_c.asset_name);

         FETCH get_asset_cost INTO ln_asset_cost;

         CLOSE get_asset_cost;

         OPEN get_asset_id (lcu_cta_asset_c.asset_name);

         FETCH get_asset_id
         INTO ln_asset_id, lc_book_type_code;

         CLOSE get_asset_id;

         OPEN get_salvage_value_c (lcu_cta_asset_c.kle_id);

         FETCH get_salvage_value_c INTO ln_salvage_value;

         CLOSE get_salvage_value_c;

         l_asset_hdr_rec.asset_id := ln_asset_id;
         l_asset_hdr_rec.book_type_code := lc_book_type_code;
         l_asset_fin_rec_adj.COST := ln_asset_cost;
         l_asset_fin_rec_adj.salvage_value := ln_salvage_value;
         --l_trans_rec.transaction_subtype := 'AMORTIZED';
         fa_adjustment_pub.do_adjustment (
            p_api_version                 => 1.0,
            p_init_msg_list               => fnd_api.g_false,
            p_commit                      => fnd_api.g_false,
            p_validation_level            => fnd_api.g_valid_level_full,
            x_return_status               => l_return_status2,
            x_msg_count                   => l_mesg_count,
            x_msg_data                    => l_mesg,
            p_calling_fn                  => 'ADJ_TEST_SCRIPT',
            px_trans_rec                  => l_trans_rec,
            px_asset_hdr_rec              => l_asset_hdr_rec,
            p_asset_fin_rec_adj           => l_asset_fin_rec_adj,
            x_asset_fin_rec_new           => l_asset_fin_rec_new,
            x_asset_fin_mrc_tbl_new       => l_asset_fin_mrc_tbl_new,
            px_inv_trans_rec              => l_inv_trans_rec,
            px_inv_tbl                    => l_inv_tbl,
            px_inv_rate_tbl               => l_inv_rate_tbl,
            p_asset_deprn_rec_adj         => l_asset_deprn_rec_adj,
            x_asset_deprn_rec_new         => l_asset_deprn_rec_new,
            x_asset_deprn_mrc_tbl_new     => l_asset_deprn_mrc_tbl_new,
            p_group_reclass_options_rec   => l_group_reclass_options_rec);

         IF (l_return_status2 <> fnd_api.g_ret_sts_success)
         THEN
            l_mesg_count := fnd_msg_pub.count_msg;

            IF l_mesg_count > 0
            THEN
               l_mesg :=
                  SUBSTR (
                     fnd_msg_pub.get (fnd_msg_pub.g_first, fnd_api.g_false),
                     1,
                     512);
               fnd_file.put_line (
                  fnd_file.LOG,
                     'Error while creating FA_ADJUSTMENT for CTA asset : '
                  || lcu_cta_asset_c.asset_name
                  || ' '
                  || l_mesg);

               x_return_status := 'E';
               x_msg_data := l_mesg;
               fnd_msg_pub.delete_msg ();
            END IF;
         ELSE
            UPDATE okl_txl_assets_b
               SET depreciation_cost = ln_asset_cost,
                   original_cost = ln_asset_cost,
                   salvage_value = ln_salvage_value,
                   depreciate_yn = 'Y',
                   last_update_date = SYSDATE,
                   last_updated_by = fnd_global.user_id,
                   last_update_login = -1
             WHERE     dnz_khr_id = p_contract_id
                   AND asset_number = lcu_cta_asset_c.asset_name;

            IF (SQL%ROWCOUNT = 0)
            THEN
               l_mesg := SUBSTR (SQLERRM, 1, 512);
               x_return_status := 'E';
               x_msg_data := l_mesg;
               fnd_file.put_line (
                  fnd_file.LOG,
                     'Error while updating Salvage Value and depreciation cost in OLM for CTA asset : '
                  || lcu_cta_asset_c.asset_name
                  || ' '
                  || l_mesg);
            ELSE
               x_return_status := 'S';
            END IF;
         END IF;
      END LOOP;

      COMMIT;
   EXCEPTION
      WHEN OTHERS
      THEN
         x_return_status := 'E';
         fnd_file.put_line (fnd_file.LOG,
                            ' Unexpected error in FA adjustment creation ');
   END create_fa_adj_cta_asst_prc;

   PROCEDURE change_depreciation_flag_prc (
      p_contract_id       IN            okc_k_headers_b.ID%TYPE,
      p_rbk_contract_id   IN            okc_k_headers_b.ID%TYPE,
      x_return_status        OUT NOCOPY VARCHAR2,
      x_msg_data             OUT NOCOPY VARCHAR2)
   IS
      CURSOR get_assets_on_contract_c (
         p_id okc_k_headers_b.ID%TYPE)
      IS
         SELECT faa.asset_id asset_id, fab.book_type_code corp_book
           FROM apps.fa_additions_b faa,
                apps.fa_books fab,
                apps.fa_book_controls fbc,
                apps.okl_txl_assets_b ota
          WHERE     ota.dnz_khr_id = p_id
                AND faa.asset_number = ota.asset_number
                AND faa.asset_id = fab.asset_id
                AND fab.date_ineffective IS NULL
                AND fab.transaction_header_id_out IS NULL
                AND fab.book_type_code = fbc.book_type_code
                AND fbc.book_class = 'CORPORATE'
                AND NVL (TRUNC (fbc.date_ineffective), TRUNC (SYSDATE) + 1) >
                       TRUNC (SYSDATE);

      CURSOR get_asset_orgnl_cost_c (
         p_asset_id NUMBER)
      IS
         SELECT DISTINCT
                FIRST_VALUE (a.original_cost) OVER (ORDER BY id DESC)
                   original_cost
           FROM okl_txl_assets_b a, fa_additions_b b
          WHERE     a.asset_number = b.asset_number
                AND b.asset_id = p_asset_id
                AND a.dnz_khr_id = p_rbk_contract_id;


      lcu_get_assets_on_contract_rec   get_assets_on_contract_c%ROWTYPE;
      ln_asset_orig_cost               fa_books.original_cost%TYPE;
   BEGIN
      -- Assigning variables
      lcu_get_assets_on_contract_rec := NULL;
      ln_asset_orig_cost := 0;

      -- Open cursor to fetch the asset lines
      OPEN get_assets_on_contract_c (p_contract_id);

     <<assets_on_contract>>
      LOOP
         FETCH get_assets_on_contract_c INTO lcu_get_assets_on_contract_rec;

         EXIT assets_on_contract WHEN get_assets_on_contract_c%NOTFOUND;

         OPEN get_asset_orgnl_cost_c (lcu_get_assets_on_contract_rec.asset_id);

         FETCH get_asset_orgnl_cost_c INTO ln_asset_orig_cost;

         EXIT WHEN get_asset_orgnl_cost_c%NOTFOUND;

         CLOSE get_asset_orgnl_cost_c;

         -- Calling API to set the depreciation flag.
         fa_books_pkg.update_row (
            x_asset_id         => lcu_get_assets_on_contract_rec.asset_id,
            x_book_type_code   => lcu_get_assets_on_contract_rec.corp_book,
            x_original_cost    => ln_asset_orig_cost,
            --            x_depreciate_flag   => 'YES',
            x_calling_fn       => 'TEST');

         COMMIT;
      END LOOP assets_on_contract;

      x_return_status := 'S';

      CLOSE get_assets_on_contract_c;
   EXCEPTION
      WHEN OTHERS
      THEN
         x_return_status := 'E';
         fnd_file.put_line (fnd_file.LOG, ' Unexpected error in FA update ');
   END change_depreciation_flag_prc;

   PROCEDURE change_depreciation_flag_prc2 (
      p_contract_id     IN            okc_k_headers_b.ID%TYPE,
      x_return_status      OUT NOCOPY VARCHAR2,
      x_msg_data           OUT NOCOPY VARCHAR2)
   IS
      CURSOR get_assets_on_contract_c (
         p_id okc_k_headers_b.ID%TYPE)
      IS
         SELECT faa.asset_id asset_id, fab.book_type_code corp_book
           FROM apps.fa_additions_b faa,
                apps.fa_books fab,
                apps.fa_book_controls fbc,
                apps.okl_txl_assets_b ota
          WHERE     ota.dnz_khr_id = p_id
                AND faa.asset_number = ota.asset_number
                AND faa.asset_id = fab.asset_id
                AND fab.date_ineffective IS NULL
                AND fab.transaction_header_id_out IS NULL
                AND fab.book_type_code = fbc.book_type_code
                AND fbc.book_class = 'CORPORATE'
                AND NVL (TRUNC (fbc.date_ineffective), TRUNC (SYSDATE) + 1) >
                       TRUNC (SYSDATE);

      lcu_get_assets_on_contract_rec   get_assets_on_contract_c%ROWTYPE;


      --Start modification by Sherin for OL Bug#267 dated 21st Oct ver RelOL_1.35
      CURSOR get_exs_ccid_c (
         p_asset_id NUMBER)
      IS
         SELECT fda.row_id,
                fda.asset_id,
                fda.distribution_id,
                fda.code_combination_id,
                fda.book_type_code,
                gcc.segment1,
                gcc.segment2,
                gcc.segment3,
                gcc.segment4,
                gcc.segment5,
                gcc.segment6,
                gcc.segment7,
                fda.Units_Assigned,
                fda.Date_Effective,
                fda.Location_Id,
                fda.Transaction_Header_Id_In,
                fda.Last_Update_Date,
                fda.Last_Updated_By,
                fda.Date_Ineffective,
                fab.transaction_header_id_in book_header_id,
                fda.transaction_header_id_out
           FROM fa_distribution_history_v fda,
                gl_code_combinations gcc,
                fa_books fab,
                fa_book_controls fbc
          WHERE     fda.asset_id = p_asset_id
                AND fda.code_combination_id = gcc.code_combination_id
                AND fab.asset_id = fda.asset_id
                AND fab.book_type_code = fbc.book_type_code
                AND fbc.book_class = 'CORPORATE'            --'DLL US LS CORP'
                AND fab.date_ineffective IS NULL;

      CURSOR get_asset_dtls_c (
         p_asset_id NUMBER)
      IS
         SELECT okl.dnz_chr_id,
                fab.asset_id,
                xxd_okl_custom_fn_x_pk.check_syndicated_flag_fnc (
                   okl.dnz_chr_id)
                   synd_yn,
                reporting_entity,
                sbu
           FROM fa_additions_b fab,
                okc_k_lines_v okl,
                okc_line_styles_b lse,
                xxd_okl_k_headers_t xxd
          WHERE     fab.asset_id = p_asset_id
                AND fab.asset_number = okl.name
                AND okl.lse_id = lse.id
                AND lse.lty_code = 'FREE_FORM1'
                AND okl.sts_code IN ('BOOKED', 'EVERGREEN')
                AND okl.dnz_chr_id = xxd.khr_id;

      CURSOR get_coa_c (p_sob_id NUMBER)
      IS
         SELECT chart_of_accounts_id
           FROM gl_sets_of_books
          WHERE set_of_books_id = p_sob_id;

      CURSOR get_cur_per_c (
         p_asset_id NUMBER)
      IS
         SELECT calendar_period_open_date
           FROM fa_deprn_periods fdp, fa_book_controls fbc, fa_books fab
          WHERE     fdp.book_type_code = fbc.book_type_code
                AND fbc.book_class = 'CORPORATE'
                AND period_close_date IS NULL
                AND fab.asset_id = p_asset_id
                AND fab.book_type_code = fdp.book_type_code
                AND fab.date_ineffective IS NULL;

      ln_contract_id                   NUMBER;
      ln_asset_name                    VARCHAR2 (30) := NULL;
      lc_synd_flag                     VARCHAR2 (10) := 'N';
      ln_rep_entity                    NUMBER := NULL;
      lc_conc_seg                      VARCHAR2 (40) := NULL;
      ln_coa                           NUMBER := NULL;
      ln_ccid                          NUMBER := NULL;
      lc_delimiter                     VARCHAR2 (10) := NULL;
      lc_trans_hdr_id                  NUMBER := NULL;
      lc_hdr_rowid                     VARCHAR2 (50) := NULL;
      lc_rowid                         VARCHAR2 (50) := NULL;
      lc_trans_type_code               VARCHAR2 (50) := 'TRANSFER';
      ln_book_hdr_id                   NUMBER; -- generated from fa_tran_hdr api
      ln_distribution_id               NUMBER; --distribution id for the new fa_dist_hist entry
      l_return_status                  BOOLEAN;
      ld_open_per                      DATE;
      ld_date                          DATE;
      p_asset_id                       NUMBER;
      lc_sbu                           VARCHAR2 (30);

      lcu_get_exs_ccid_rec             get_exs_ccid_c%ROWTYPE;
   --End modification by Sherin for OL Bug#267 dated 21st Oct ver RelOL_1.35
   BEGIN
      -- Assigning variables
      lcu_get_assets_on_contract_rec := NULL;

      -- Open cursor to fetch the asset lines
      OPEN get_assets_on_contract_c (p_contract_id);

     <<assets_on_contract>>
      LOOP
         FETCH get_assets_on_contract_c INTO lcu_get_assets_on_contract_rec;

         EXIT assets_on_contract WHEN get_assets_on_contract_c%NOTFOUND;

         -- Calling API to set the depreciation flag.
         fa_books_pkg.update_row (
            x_asset_id          => lcu_get_assets_on_contract_rec.asset_id,
            x_book_type_code    => lcu_get_assets_on_contract_rec.corp_book,
            x_depreciate_flag   => 'YES',
            x_calling_fn        => 'TEST');

         COMMIT;

         --Start modification by Sherin for OL Bug#267 dated 21st Oct ver RelOL_1.35
         gn_sob_id := fnd_profile.VALUE ('gl_set_of_bks_id');
         p_asset_id := lcu_get_assets_on_contract_rec.asset_id;

         OPEN get_cur_per_c (p_asset_id);

         FETCH get_cur_per_c INTO ld_open_per;

         CLOSE get_cur_per_c;

         OPEN get_asset_dtls_c (p_asset_id);

         FETCH get_asset_dtls_c
         INTO ln_contract_id,
              ln_asset_name,
              lc_synd_flag,
              ln_rep_entity,
              lc_sbu;

         CLOSE get_asset_dtls_c;

         OPEN get_coa_c (gn_sob_id);

         FETCH get_coa_c INTO ln_coa;

         CLOSE get_coa_c;

         lcu_get_exs_ccid_rec := NULL;

         OPEN get_exs_ccid_c (p_asset_id);

         --        <<exs_ccid_dtls>>
         --         LOOP
         FETCH get_exs_ccid_c INTO lcu_get_exs_ccid_rec;

         --            EXIT exs_ccid_dtls WHEN get_exs_ccid_c%NOTFOUND;

         IF (lc_synd_flag = 'Y')
         THEN
            --Added new condition to avoid repeating/duplicate lines getting created
            IF (   ln_rep_entity <> lcu_get_exs_ccid_rec.segment1
                OR lc_sbu <> lcu_get_exs_ccid_rec.segment2)
            THEN
               lc_trans_hdr_id := NULL;
               lc_hdr_rowid := NULL;
               ln_distribution_id := NULL;
               lc_rowid := NULL;

               lc_delimiter :=
                  fnd_flex_ext.get_delimiter (
                     application_short_name   => 'SQLGL',
                     key_flex_code            => 'GL#',
                     structure_number         => ln_coa);

               ln_ccid :=
                  fnd_flex_ext.get_ccid (
                     application_short_name   => 'SQLGL',
                     key_flex_code            => 'GL#',
                     structure_number         => ln_coa,
                     validation_date          => TO_CHAR (
                                                   SYSDATE,
                                                   apps.fnd_flex_ext.DATE_FORMAT),
                     concatenated_segments    =>    ln_rep_entity
                                                 || lc_delimiter
                                                 || lc_sbu --lcu_get_exs_ccid_rec.segment2
                                                 || lc_delimiter
                                                 || lcu_get_exs_ccid_rec.segment3
                                                 || lc_delimiter
                                                 || lcu_get_exs_ccid_rec.segment4
                                                 || lc_delimiter
                                                 || lcu_get_exs_ccid_rec.segment5
                                                 || lc_delimiter
                                                 || lcu_get_exs_ccid_rec.segment6
                                                 || lc_delimiter
                                                 || lcu_get_exs_ccid_rec.segment7);

               fnd_file.put_line (fnd_file.LOG,
                                  'Generated ccid: ' || ln_ccid);
               fnd_file.put_line (
                  fnd_file.LOG,
                     'Inserting a new TRANSFER entry into FA_TRANSACTION_HEADERS : '
                  || ln_ccid);

               IF NVL (ln_ccid, 0) <> 0
               THEN
                  FA_TRANSFERS_PKG.Insert_Header (
                     X_Rowid                          => lc_hdr_rowid,
                     X_Transaction_Header_Id          => lc_trans_hdr_id,
                     X_Book_Type_Code                 => lcu_get_exs_ccid_rec.book_type_code,
                     X_Asset_Id                       => lcu_get_exs_ccid_rec.asset_id,
                     X_Transaction_Type_Code          => lc_trans_type_code,
                     X_Transaction_Date_Entered       => SYSDATE,
                     X_Date_Effective                 => ld_date,
                     X_Last_Update_Date               => SYSDATE,
                     X_Last_Updated_By                => gn_user_id,
                     X_Transaction_Name               => NULL,
                     X_Invoice_Transaction_Id         => NULL,
                     X_Source_Transaction_Header_Id   => NULL,
                     X_Mass_Reference_Id              => NULL,
                     X_Last_Update_Login              => gn_user_id,
                     X_Transaction_Subtype            => NULL,
                     X_Attribute1                     => NULL,
                     X_Attribute2                     => NULL,
                     X_Attribute3                     => NULL,
                     X_Attribute4                     => NULL,
                     X_Attribute5                     => NULL,
                     X_Attribute6                     => NULL,
                     X_Attribute7                     => NULL,
                     X_Attribute8                     => NULL,
                     X_Attribute9                     => NULL,
                     X_Attribute10                    => NULL,
                     X_Attribute11                    => NULL,
                     X_Attribute12                    => NULL,
                     X_Attribute13                    => NULL,
                     X_Attribute14                    => NULL,
                     X_Attribute15                    => NULL,
                     X_Attribute_Category_Code        => NULL,
                     X_Transaction_Key                => NULL,
                     X_Book_Header_Id                 => lcu_get_exs_ccid_rec.book_header_id, -- ln_book_hdr_id ,
                     X_Retirement_Id                  => NULL,
                     X_Calendar_Period_Open_Date      => ld_open_per,
                     X_return_status                  => l_return_status,
                     X_Calling_Fn                     => 'XXD_OL_RBK_PKG.CHANGE_DEPRECIATION_FLAG_PRC2');
                  COMMIT;

                  IF (NOT l_return_status)
                  THEN
                     fnd_file.put_line (
                        fnd_file.LOG,
                        ' The FA_TRANSACTION_HEADERS insert failed');
                  ELSE
                     fnd_file.put_line (
                        fnd_file.LOG,
                           ' The FA_TRANSACTION_HEADERS insert was successful - Transaction Header ID : '
                        || lc_trans_hdr_id);
                  END IF;


                  fnd_file.put_line (
                     fnd_file.LOG,
                     'Updating the existing distribution history entry with the new transaction_hdr_id and making it ineffective');


                  FA_TRANSFERS_PKG.Update_Dist (
                     X_Rowid                       => lcu_get_exs_ccid_rec.row_id,
                     X_Distribution_Id             => lcu_get_exs_ccid_rec.distribution_id,
                     X_Book_Type_Code              => lcu_get_exs_ccid_rec.book_type_code,
                     X_Asset_Id                    => lcu_get_exs_ccid_rec.asset_id,
                     X_Units_Assigned              => lcu_get_exs_ccid_rec.units_assigned,
                     X_Date_Effective              => lcu_get_exs_ccid_rec.date_effective,
                     X_Code_Combination_Id         => lcu_get_exs_ccid_rec.code_combination_id,
                     X_Location_Id                 => lcu_get_exs_ccid_rec.location_id,
                     X_Transaction_Header_Id_In    => lcu_get_exs_ccid_rec.transaction_header_id_in,
                     X_Last_Update_Date            => SYSDATE,
                     X_Last_Updated_By             => gn_user_id,
                     X_Date_Ineffective            => SYSDATE,
                     X_Assigned_To                 => NULL,
                     X_Transaction_Header_Id_Out   => lc_trans_hdr_id,
                     X_Transaction_Units           => -1,
                     X_Retirement_Id               => NULL,
                     X_Last_Update_Login           => NULL,
                     X_Book_Header_Id              => lcu_get_exs_ccid_rec.book_header_id,
                     X_Return_Status               => l_return_status,
                     X_Calling_Fn                  => 'XXD_OL_RBK_PKG.CHANGE_DEPRECIATION_FLAG_PRC2');

                  COMMIT;

                  IF (NOT l_return_status)
                  THEN
                     fnd_file.put_line (
                        fnd_file.LOG,
                        ' The FA_DISTRIBUTION_HISTORY update for the existing dist ID failed');
                  ELSE
                     fnd_file.put_line (
                        fnd_file.LOG,
                           ' The FA_DISTRIBUTION_HISTORY update was successful and transaction Hdr ID out was updated as : '
                        || lc_trans_hdr_id);
                  END IF;

                  fnd_file.put_line (
                     fnd_file.LOG,
                     'Inserting a new entry into FA_DISTRIBUTION_HISTORY with the newly derived ccid');

                  FA_TRANSFERS_PKG.Insert_Dist (
                     X_Rowid                       => lc_rowid,
                     X_Distribution_Id             => ln_distribution_id,
                     X_Book_Type_Code              => lcu_get_exs_ccid_rec.book_type_code,
                     X_Asset_Id                    => lcu_get_exs_ccid_rec.asset_id,
                     X_Units_Assigned              => lcu_get_exs_ccid_rec.units_assigned,
                     X_Date_Effective              => SYSDATE,
                     X_Code_Combination_Id         => ln_ccid,
                     X_Location_Id                 => lcu_get_exs_ccid_rec.location_id,
                     X_Transaction_Header_Id_In    => lc_trans_hdr_id,
                     X_Last_Update_Date            => SYSDATE,
                     X_Last_Updated_By             => gn_user_id,
                     X_Date_Ineffective            => NULL,
                     X_Assigned_To                 => NULL,
                     X_Transaction_Header_Id_Out   => NULL,
                     X_Transaction_Units           => 1,
                     X_Retirement_Id               => NULL,
                     X_Last_Update_Login           => NULL,
                     X_Book_Header_Id              => lcu_get_exs_ccid_rec.book_header_id,
                     X_Return_Status               => l_return_status,
                     X_Calling_Fn                  => 'XXD_OL_RBK_PKG.CHANGE_DEPRECIATION_FLAG_PRC2');
                  COMMIT;


                  IF (NOT l_return_status)
                  THEN
                     fnd_file.put_line (
                        fnd_file.LOG,
                        ' The FA_DISTRIBUTION_HISTORY insert for the new line failed');
                  ELSE
                     fnd_file.put_line (
                        fnd_file.LOG,
                           ' The FA_DISTRIBUTION_HISTORY insert was successful and Distribution ID is : '
                        || ln_distribution_id);
                  END IF;
               ELSE
                  fnd_file.put_line (fnd_file.LOG, ' CCID generated is 0');
               END IF;
            END IF;
         END IF;

         --         END LOOP exis_ccid_dtls;

         CLOSE get_exs_ccid_c;

         COMMIT;
      --End modification by Sherin for OL Bug#267 dated 21st Oct ver RelOL_1.35
      END LOOP assets_on_contract;

      x_return_status := 'S';

      CLOSE get_assets_on_contract_c;
   EXCEPTION
      WHEN OTHERS
      THEN
         x_return_status := 'E';
         fnd_file.put_line (fnd_file.LOG, ' Unexpected error in FA update ');
   END change_depreciation_flag_prc2;

   PROCEDURE xxd_cancel_rbk_qte_req (p_contract_id      IN     NUMBER,
                                     p_conc_req_id      IN     NUMBER,
                                     p_ret_status          OUT VARCHAR2,
                                     p_qte_ret_status      OUT VARCHAR2,
                                     p_rbk_flag            OUT VARCHAR2,
                                     p_qte_flag            OUT VARCHAR2)
   IS
      CURSOR get_rbk_req_c (
         p_contract_id NUMBER)
      IS
         SELECT okh.contract_number, okh.id khr_id, otr.request_status_code
           FROM okl_trx_requests otr, okc_k_headers_b okh
          WHERE     1 = 1
                AND otr.dnz_khr_id = okh.id
                AND request_type_code = 'DLL_PROPOSED_RB_REQ'
                AND okh.id = p_contract_id;

      CURSOR get_qte_req_c (
         p_contract_id NUMBER)
      IS
         SELECT hdr.contract_number,
                qte.id,
                quote_number,
                qst_code,                                           --validate
                khr_id
           FROM okl_trx_quotes_b qte, okc_k_headers_b hdr
          WHERE     qst_code NOT IN ('CANCELLED', 'REJECTED', 'COMPLETE')
                AND date_effective_to >= SYSDATE
                AND khr_id = p_contract_id
                AND qte.khr_id = hdr.id;


      lt_rev_tbl         okl_transaction_pub.rev_tbl_type;
      gc_return_status   VARCHAR2 (1);
      gc_error_message   xxd_geh_msgs_t.MESSAGE_TEXT%TYPE;
      gn_msg_count       NUMBER;
   BEGIN
      p_ret_status := 'S';
      p_rbk_flag := 'N';

      FOR lcu_get_rbk_req_rec IN get_rbk_req_c (p_contract_id)
      LOOP
         EXIT WHEN get_rbk_req_c%NOTFOUND;

         IF (lcu_get_rbk_req_rec.request_status_code = 'APPROVED')
         THEN
            p_rbk_flag := 'Y';
            fnd_file.put_line (
               fnd_file.LOG,
                  'The contract '
               || lcu_get_rbk_req_rec.contract_number
               || ' has a Pending Rebook Request');

            UPDATE xxd_ol_rbk_temp_t
               SET status = 'E', error_msg = 'Rebook Request Exists'
             WHERE     contract_number = lcu_get_rbk_req_rec.contract_number
                   AND request_id = p_conc_req_id;

            COMMIT;
            p_ret_status := 'P';
         ELSE
            p_rbk_flag := 'N';

            UPDATE okl_trx_requests
               SET request_status_code = 'CANCELLED'
             WHERE     dnz_khr_id = p_contract_id
                   AND request_type_code = 'DLL_PROPOSED_RB_REQ'
                   AND request_status_code = 'APPROVED';

            COMMIT;
            p_ret_status := 'S';
         END IF;
      END LOOP;

      p_qte_ret_status := 'S';
      p_qte_flag := 'N';

      FOR lcu_get_qte_req_rec IN get_qte_req_c (p_contract_id)
      LOOP
         EXIT WHEN get_qte_req_c%NOTFOUND;

         IF (lcu_get_qte_req_rec.qst_code IN ('APPROVED', 'ACCEPTED'))
         THEN
            p_qte_flag := 'Y';
            fnd_file.put_line (
               fnd_file.LOG,
                  'The contract '
               || lcu_get_qte_req_rec.contract_number
               || ' has a Pending Quote');

            UPDATE xxd_ol_rbk_temp_t
               SET status = 'E', error_msg = 'Termination Quote Exists'
             WHERE     contract_number = lcu_get_qte_req_rec.contract_number
                   AND request_id = p_conc_req_id;

            COMMIT;
            p_qte_ret_status := 'P';
         ELSE
            p_qte_flag := 'N';

            BEGIN
               UPDATE okl_trx_quotes_b
                  SET qst_code = 'REJECTED'
                WHERE     khr_id = lcu_get_qte_req_rec.khr_id
                      AND id = lcu_get_qte_req_rec.id
                      AND quote_number = lcu_get_qte_req_rec.quote_number;

               IF SQL%ROWCOUNT = 0
               THEN
                  p_qte_ret_status := 'E';
                  fnd_file.put_line (fnd_file.LOG,
                                     'Failed to cancel quote requests');
               ELSE
                  p_qte_ret_status := 'S';
                  fnd_file.put_line (fnd_file.LOG,
                                     'Cancelled existing quote requests');
               END IF;

               COMMIT;
            EXCEPTION
               WHEN OTHERS
               THEN
                  p_qte_ret_status := 'E';
                  fnd_file.put_line (
                     fnd_file.LOG,
                     'Error while cancelling termination quote request');
            END;
         END IF;
      END LOOP;

      RETURN;
   EXCEPTION
      WHEN OTHERS
      THEN
         fnd_file.put_line (fnd_file.LOG,
                            'Error while cancelling rebook/quote requests');
   END xxd_cancel_rbk_qte_req;

   PROCEDURE xxd_upd_temp_tab_proc (
      p_temp_tab_rec IN xxd_ol_rbk_temp_t%ROWTYPE --      x_return_status      OUT VARCHAR2
                                                 )
   IS
      PRAGMA AUTONOMOUS_TRANSACTION;
      l_temp_tab_rec   xxd_ol_rbk_temp_t%ROWTYPE;
   BEGIN
      l_temp_tab_rec := p_temp_tab_rec;
      fnd_file.put_line (fnd_file.LOG, ' Updating the temp table');

      UPDATE xxd_ol_rbk_temp_t
         SET BATCH_ID = l_temp_tab_rec.batch_id,
             BATCH_DATE = l_temp_tab_rec.batch_date,
             CONTRACT_ID = l_temp_tab_rec.contract_id,
             CONTRACT_NUMBER = l_temp_tab_rec.contract_number,
             ORIG_PDT_ID = l_temp_tab_rec.orig_pdt_id,
             CHILD_NUM = l_temp_tab_rec.child_num,
             ENTITY = l_temp_tab_rec.entity,
             START_DATE = l_temp_tab_rec.start_date,
             END_DATE = l_temp_tab_rec.end_date,
             STS_CODE = l_temp_tab_rec.sts_code,
             CLASS = l_temp_tab_rec.class,
             ASSET_CNT = l_temp_tab_rec.asset_cnt,
             TOT_ASST_COST = l_temp_tab_rec.tot_asst_cost,
             TOT_SLVG_VAL = l_temp_tab_rec.tot_slvg_val,
             INCENTIVE_AMT = l_temp_tab_rec.incentive_amt,
             STATUS = l_temp_tab_rec.status,
             PROCESSED_FLAG = l_temp_tab_rec.processed_flag,
             ORG_ID = l_temp_tab_rec.org_id,
             CREATION_DATE = l_temp_tab_rec.creation_date,
             REQUEST_ID = l_temp_tab_rec.request_id,
             ERROR_MSG = l_temp_tab_rec.error_msg,
             TOT_RES_VAL = l_temp_tab_rec.tot_res_val,
             TOT_ADD_ON = l_temp_tab_rec.tot_add_on,
             TOT_CAP_FEE = l_temp_tab_rec.tot_cap_fee,
             TOT_ROL_FEE = l_temp_tab_rec.tot_rol_fee,
             TOT_WARRANTY = l_temp_tab_rec.tot_warranty,
             TOT_MAINT = l_temp_tab_rec.tot_maint,
             REV_DFL_UN_LEASE_INC = l_temp_tab_rec.rev_dfl_un_lease_inc,
             REV_DFL_ACC_LEASE_INC = l_temp_tab_rec.rev_dfl_acc_lease_inc,
             REV_DFL_NAC_LEASE_INC = l_temp_tab_rec.rev_dfl_nac_lease_inc,
             REV_DFL_UN_RES_INC = l_temp_tab_rec.rev_dfl_un_res_inc,
             REV_DFL_ACC_RES_INC = l_temp_tab_rec.rev_dfl_acc_res_inc,
             REV_DFL_NAC_RES_INC = l_temp_tab_rec.rev_dfl_nac_res_inc,
             REV_DFL_UNBLD_REC = l_temp_tab_rec.rev_dfl_unbld_rec,
             REV_DFL_BROKER_FEE = l_temp_tab_rec.rev_dfl_broker_fee,
             REV_DFL_ACQ_FEE = l_temp_tab_rec.rev_dfl_acq_fee,
             REV_DFL_SUBSIDY = l_temp_tab_rec.rev_dfl_subsidy,
             REV_DFL_IDC = l_temp_tab_rec.rev_dfl_idc,
             REV_DFL_RESIDUAL = l_temp_tab_rec.rev_dfl_residual,
             ACCRUAL_RENT_INC = l_temp_tab_rec.accrual_rent_inc,
             NON_ACCRUAL_TOT = l_temp_tab_rec.non_accrual_tot,
             ACCUM_DEPRN = l_temp_tab_rec.accum_deprn,
             TOTAL_ACCRUAL = l_temp_tab_rec.total_accrual
       WHERE     contract_id = l_temp_tab_rec.contract_id
             AND request_id = l_temp_tab_rec.request_id;

      COMMIT;
   EXCEPTION
      WHEN OTHERS
      THEN
         fnd_file.put_line (
            fnd_file.LOG,
            'Unexpected error in the temp table update procedure');
   END xxd_upd_temp_tab_proc;

   PROCEDURE xxd_upd_asset_values (p_contract_id     IN     NUMBER,
                                   p_rbk_chr_id             NUMBER,
                                   p_request_id             NUMBER,
                                   p_return_status      OUT VARCHAR2)
   IS
      ln_units                  NUMBER := 0;
      ln_asset_cost             NUMBER := 0;
      ln_orig_cost              NUMBER := 0;
      ln_addon_fee              NUMBER := 0;
      ln_cap_rol_fee            NUMBER := 0;
      ln_cap_fee                NUMBER := 0;
      ln_rol_fee                NUMBER := 0;
      ln_asset_addon            NUMBER := 0;
      ln_maintenance            NUMBER := 0;
      ln_warranty               NUMBER := 0;
      --      ln_accum_deprn            NUMBER := 0;
      --      ln_rental_accrual         NUMBER := 0;
      --      ln_rental_non_accrual     NUMBER := 0;

      --totals
      ln_tot_asst_cnt           NUMBER := 0;
      ln_tot_asst_cost          NUMBER := 0;
      ln_tot_slvg_val           NUMBER := 0;
      ln_tot_cap_fee            NUMBER := 0;
      ln_tot_rol_fee            NUMBER := 0;
      ln_tot_addon              NUMBER := 0;
      ln_tot_mntnc              NUMBER := 0;
      ln_tot_wrnty              NUMBER := 0;
      --      ln_tot_accum_deprn        NUMBER := 0;
      ln_tot_incentive_amt      NUMBER := 0;
      --      ln_tot_accrual            NUMBER := 0;

      --variables for the update procedure

      lc_return_status          VARCHAR2 (1);
      ln_message_count          NUMBER;
      -- Initializing local variables for debugging framework
      lc_function_name          VARCHAR2 (100) := 'ASSIGN_CORP_BOOK_ATTRS';
      lc_begin_debug            VARCHAR2 (100)
                                   := 'Begin Debug ' || lc_function_name;
      lc_end_debug              VARCHAR2 (100)
                                   := 'End Debug ' || lc_function_name;

      lc_init                   VARCHAR2 (10) DEFAULT xxd_api.gc_false;
      lc_msg_data               VARCHAR2 (2000);
      lc_method                 fa_category_book_defaults.deprn_method%TYPE;
      lc_life                   VARCHAR2 (100);
      ln_depr_cost              NUMBER;
      lc_book                   VARCHAR2 (100);
      --      p_talv_rec_type      IN             okl_tal_pvt.talv_rec_type
      l_talv_rec_type           okl_tal_pvt.talv_rec_type;
      l_talv_rec_o_type         okl_tal_pvt.talv_rec_type;
      ln_msg_index_out_o        NUMBER;


      CURSOR get_assets_c (
         p_contract_id NUMBER)
      IS
         SELECT a.id asset_id, a.name asset_name
           FROM okc_k_lines_v a, okc_line_styles_b b
          WHERE     a.dnz_chr_id = p_contract_id
                AND a.lse_id = b.id                                       --33
                AND b.lty_code = 'FREE_FORM1'
                AND a.sts_code = 'BOOKED';

      CURSOR get_asset_costs_c (p_kle_id VARCHAR2)
      IS
         SELECT residual_value, oec unit_cost
           FROM OKL_K_LINES a
          WHERE id = p_kle_id;

      CURSOR get_asset_unit_cost (
         p_kle_id VARCHAR2)
      IS
         SELECT price_unit
           FROM okc_k_lines_b oklb, okc_line_styles_b olsb
          WHERE     oklb.lse_id = olsb.ID
                AND oklb.cle_id = p_kle_id
                AND olsb.lty_code = 'ITEM';

      CURSOR get_asset_units_c (p_contract_id NUMBER, -- do we make this the rebook chr_id ?
                                                     p_asset_name VARCHAR2)
      IS
         SELECT current_units
           FROM okl_txl_assets_b
          WHERE dnz_khr_id = p_contract_id                      --p_rbk_chr_id
                                          AND asset_number = p_asset_name;

      --                AND tal_type = 'CFA';

      CURSOR get_asset_id (p_contract_id2 NUMBER, p_asset_name VARCHAR2)
      IS
         SELECT id
           FROM okl_txl_assets_b
          WHERE dnz_khr_id = p_contract_id2 AND asset_number = p_asset_name;

      CURSOR get_asset_txd_id (p_tal_id NUMBER)
      IS
         SELECT id
           FROM okl_txd_assets_b
          WHERE tal_id = p_tal_id;

      CURSOR get_cap_fee_c (
         p_contract_id    NUMBER,
         p_kle_id         VARCHAR2)
      IS
           SELECT NVL (SUM (kle2.capital_amount), 0) amount, orig_asset.name
             FROM okc_k_headers_b hdr,
                  okc_k_lines_b cle,
                  okl_k_lines kle,
                  okc_line_styles_b lse,
                  okc_k_items item,
                  okl_k_lines kle2,
                  okc_k_lines_b cleasst,
                  okc_line_styles_b lseasst,
                  okl_strm_type_b ssv,
                  okc_k_items itemasst,
                  okc_k_lines_v orig_asset
            WHERE     cle.ID = kle.ID
                  AND cle.cle_id IS NULL
                  AND cle.lse_id = lse.ID
                  AND lse.lty_code = 'FEE'
                  AND item.cle_id = cle.ID
                  AND kle.fee_type = 'CAPITALIZED'
                  AND cleasst.cle_id = cle.id
                  AND kle2.id = cleasst.id
                  AND (    ssv.ID = item.object1_id1
                       AND item.jtot_object1_code = 'OKL_STRMTYP')
                  AND cleasst.cle_id = cle.ID
                  AND cleasst.lse_id = lseasst.ID
                  AND lseasst.lty_code = 'LINK_FEE_ASSET'
                  AND itemasst.cle_id = cleasst.ID
                  AND itemasst.jtot_object1_code = 'OKX_COVASST'
                  AND itemasst.object1_id1 = orig_asset.id
                  AND orig_asset.dnz_chr_id = hdr.id
                  AND hdr.id = cle.dnz_chr_id
                  AND hdr.id = p_contract_id
                  AND orig_asset.id = p_kle_id
         GROUP BY orig_asset.name;

      CURSOR get_rolvr_fee_c (
         p_contract_id    NUMBER,
         p_kle_id         VARCHAR2)
      IS
           SELECT NVL (SUM (kle2.amount), 0) amount, orig_asset.name
             FROM okc_k_headers_b hdr,
                  okc_k_lines_b cle,
                  okl_k_lines kle,
                  okc_line_styles_b lse,
                  okc_k_items item,
                  okl_k_lines kle2,
                  okc_k_lines_b cleasst,
                  okc_line_styles_b lseasst,
                  okl_strm_type_b ssv,
                  okc_k_items itemasst,
                  okc_k_lines_v orig_asset
            WHERE     cle.ID = kle.ID
                  AND cle.cle_id IS NULL
                  AND cle.lse_id = lse.ID
                  AND lse.lty_code = 'FEE'
                  AND item.cle_id = cle.ID
                  AND kle.fee_type = 'ROLLOVER'
                  AND cleasst.cle_id = cle.id
                  AND kle2.id = cleasst.id
                  AND (    ssv.ID = item.object1_id1
                       AND item.jtot_object1_code = 'OKL_STRMTYP')
                  AND cleasst.cle_id = cle.ID
                  AND cleasst.lse_id = lseasst.ID
                  AND lseasst.lty_code = 'LINK_FEE_ASSET'
                  AND itemasst.cle_id = cleasst.ID
                  AND itemasst.jtot_object1_code = 'OKX_COVASST'
                  AND itemasst.object1_id1 = orig_asset.id
                  AND orig_asset.dnz_chr_id = hdr.id
                  AND hdr.id = cle.dnz_chr_id
                  AND hdr.id = p_contract_id
                  AND orig_asset.id = p_kle_id
         GROUP BY orig_asset.name;

      CURSOR get_addon_c (
         p_contract_id    NUMBER,
         p_kle_id         VARCHAR2)
      IS
           SELECT SUM (addon_cle.price_unit) price, asset_cle.id
             FROM okc_k_lines_b asset_cle,
                  okc_line_styles_b asset_lse,
                  okc_k_lines_b item_cle,
                  okc_line_styles_b item_lse,
                  okc_k_lines_b addon_cle,
                  okc_line_styles_b addon_lse,
                  okc_k_items cim
            WHERE     asset_cle.dnz_chr_id = p_contract_id
                  AND asset_cle.ID = p_kle_id
                  AND asset_cle.cle_id IS NULL
                  AND asset_cle.lse_id = asset_lse.ID
                  AND asset_lse.lty_code = 'FREE_FORM1'
                  AND item_cle.dnz_chr_id = asset_cle.dnz_chr_id
                  AND item_cle.cle_id = asset_cle.ID
                  AND item_cle.lse_id = item_lse.ID
                  AND item_lse.lty_code = 'ITEM'
                  AND addon_cle.dnz_chr_id = item_cle.dnz_chr_id
                  AND addon_cle.cle_id = item_cle.ID
                  AND addon_cle.lse_id = addon_lse.ID
                  AND addon_lse.lty_code = 'ADD_ITEM'
                  AND addon_cle.ID = cim.cle_id
                  AND cim.dnz_chr_id = addon_cle.dnz_chr_id
                  AND cim.jtot_object1_code = 'OKX_SYSITEM'
         GROUP BY asset_cle.id;

      CURSOR get_maintenance_c (
         p_contract_id    NUMBER,
         p_kle_id         VARCHAR2)
      IS
         SELECT addon_cle.price_unit price,
                asset.name,
                asset_cle.id,
                mtlb.segment1
           FROM okc_k_lines_b asset_cle,
                okc_line_styles_b asset_lse,
                okc_k_lines_b item_cle,
                okc_line_styles_b item_lse,
                okc_k_lines_b addon_cle,
                okc_line_styles_b addon_lse,
                okc_k_items cim,
                mtl_system_items_b mtlb,
                mtl_system_items_tl mtlt,
                okc_k_headers_b hdr,
                okc_k_lines_v asset
          WHERE     hdr.id = p_contract_id                         --3084180--
                AND asset_cle.ID = p_kle_id
                AND asset_cle.dnz_chr_id = hdr.id
                AND asset_cle.id = asset.id
                AND asset.dnz_chr_id = hdr.id
                AND asset_cle.cle_id IS NULL
                AND asset_cle.lse_id = asset_lse.ID
                AND asset_lse.lty_code = 'FREE_FORM1'
                AND item_cle.dnz_chr_id = asset_cle.dnz_chr_id
                AND item_cle.cle_id = asset_cle.ID
                AND item_cle.lse_id = item_lse.ID
                AND item_lse.lty_code = 'ITEM'
                AND addon_cle.dnz_chr_id = item_cle.dnz_chr_id
                AND addon_cle.cle_id = item_cle.ID
                AND addon_cle.lse_id = addon_lse.ID
                AND addon_lse.lty_code = 'ADD_ITEM'
                AND addon_cle.ID = cim.cle_id
                AND cim.dnz_chr_id = addon_cle.dnz_chr_id
                AND cim.object1_id2 = TO_CHAR (mtlb.organization_id)
                AND cim.object1_id1 = mtlb.inventory_item_id
                AND cim.jtot_object1_code = 'OKX_SYSITEM'
                AND mtlb.inventory_item_id = mtlt.inventory_item_id
                AND mtlb.organization_id = mtlt.organization_id
                AND mtlb.segment1 = 'Maintenance'
                AND mtlt.LANGUAGE = 'US';

      CURSOR get_warranty_c (
         p_contract_id    NUMBER,
         p_kle_id         VARCHAR2)
      IS
         SELECT addon_cle.price_unit price,
                asset.name,
                asset_cle.id,
                mtlb.segment1
           FROM okc_k_lines_b asset_cle,
                okc_line_styles_b asset_lse,
                okc_k_lines_b item_cle,
                okc_line_styles_b item_lse,
                okc_k_lines_b addon_cle,
                okc_line_styles_b addon_lse,
                okc_k_items cim,
                mtl_system_items_b mtlb,
                mtl_system_items_tl mtlt,
                okc_k_headers_b hdr,
                okc_k_lines_v asset
          WHERE     hdr.id = p_contract_id                         --3084180--
                AND asset_cle.ID = p_kle_id
                AND asset_cle.dnz_chr_id = hdr.id
                AND asset_cle.id = asset.id
                AND asset.dnz_chr_id = hdr.id
                AND asset_cle.cle_id IS NULL
                AND asset_cle.lse_id = asset_lse.ID
                AND asset_lse.lty_code = 'FREE_FORM1'
                AND item_cle.dnz_chr_id = asset_cle.dnz_chr_id
                AND item_cle.cle_id = asset_cle.ID
                AND item_cle.lse_id = item_lse.ID
                AND item_lse.lty_code = 'ITEM'
                AND addon_cle.dnz_chr_id = item_cle.dnz_chr_id
                AND addon_cle.cle_id = item_cle.ID
                AND addon_cle.lse_id = addon_lse.ID
                AND addon_lse.lty_code = 'ADD_ITEM'
                AND addon_cle.ID = cim.cle_id
                AND cim.dnz_chr_id = addon_cle.dnz_chr_id
                AND cim.object1_id2 = TO_CHAR (mtlb.organization_id)
                AND cim.object1_id1 = mtlb.inventory_item_id
                AND cim.jtot_object1_code = 'OKX_SYSITEM'
                AND mtlb.inventory_item_id = mtlt.inventory_item_id
                AND mtlb.organization_id = mtlt.organization_id
                AND mtlb.segment1 = 'Warranty'
                AND mtlt.LANGUAGE = 'US';

      CURSOR get_orig_details (
         p_contract_id    NUMBER,
         p_request_id     NUMBER)
      IS
         SELECT op.name product_name
           FROM xxd_ol_rbk_temp_t xort, okl_products op
          WHERE     xort.orig_pdt_id = op.id
                AND xort.contract_id = p_contract_id
                AND request_id = p_request_id;

      CURSOR get_tmp_dtls_c (p_contract_id NUMBER, p_request_id NUMBER)
      IS
         SELECT *
           FROM xxd_ol_rbk_temp_t
          WHERE contract_id = p_contract_id --                AND BATCH_DATE = TRUNC (SYSDATE)
                                           AND request_id = p_request_id;

      /*      CURSOR get_accum_deprn_dtls (
               p_contract_id    NUMBER,
               p_asset_name     VARCHAR2)
            IS
               SELECT DISTINCT fab.asset_id, fbc.book_type_code
                 FROM fa_deprn_detail fds,
                      okc_k_lines_v a,
                      okc_line_styles_b b,
                      fa_book_controls fbc,
                      fa_additions_b fab
                WHERE     1 = 1
                      AND fds.asset_id = fab.asset_id
                      AND fab.asset_number = a.name
                      AND fab.asset_number = p_asset_name
                      AND fds.book_type_code = fbc.book_type_code
                      AND fbc.book_class = 'CORPORATE'
                      AND a.dnz_chr_id = p_contract_id
                      AND a.lse_id = b.id
                      AND b.lty_code = 'FREE_FORM1';

      CURSOR get_rental_accrual_c (
         p_contract_id NUMBER)
      IS
         SELECT NVL (SUM (ose.amount), 0)
           FROM okl_trx_contracts oc,
                okl_txl_cntrct_lns ocl,
                fnd_lookup_values_vl fnd,
                okl_strm_type_b sty,
                okl_streams stm,
                okl_strm_elements ose
          WHERE     1 = 1
                AND oc.khr_id = p_contract_id                       --10667459
                AND oc.tcn_type = 'ACL'
                AND oc.accrual_activity = 'ACCRUAL'
                AND oc.ID = ocl.tcn_id
                AND fnd.attribute1 = ocl.sty_id
                AND fnd.lookup_code = 'ACCRUED_LEASE_INCOME'
                AND fnd.lookup_type = 'DLL_CUSTOM_FORMULAE_STREAMS'
                AND fnd.attribute_category = 'DLL_CUSTOM_FORMULAE_STREAMS'
                AND ocl.khr_id = oc.khr_id
                AND sty.id = stm.sty_id
                AND sty.code = 'RENTAL ACCRUAL'
                AND stm.id = ose.stm_id
                AND ose.stream_element_date = oc.date_accrual
                AND stm.khr_id = oc.khr_id;

      CURSOR get_rental_non_acc_c (
         p_contract_id NUMBER)
      IS
         SELECT NVL (SUM (ose.amount), 0)
           FROM okl_trx_contracts oc,
                okl_txl_cntrct_lns ocl,
                fnd_lookup_values_vl fnd,
                okl_strm_type_b sty,
                okl_streams stm,
                okl_strm_elements ose
          WHERE     1 = 1
                AND oc.khr_id = p_contract_id                       --10667459
                AND oc.tcn_type = 'ACL'
                AND oc.accrual_activity = 'NON-ACCRUAL'
                AND oc.ID = ocl.tcn_id
                AND fnd.attribute1 = ocl.sty_id
                AND fnd.lookup_code = 'ACCRUED_LEASE_INCOME'
                AND fnd.lookup_type = 'DLL_CUSTOM_FORMULAE_STREAMS'
                AND fnd.attribute_category = 'DLL_CUSTOM_FORMULAE_STREAMS'
                AND ocl.khr_id = oc.khr_id
                AND sty.id = stm.sty_id
                AND sty.code = 'RENTAL ACCRUAL'
                AND stm.id = ose.stm_id
                AND ose.stream_element_date = oc.date_accrual
                AND stm.khr_id = oc.khr_id;

      CURSOR get_total_accrual_c (
         p_contract_id NUMBER)
      IS
         SELECT SUM (amount)
           FROM okl_streams os, okl_strm_elements ose, okl_strm_type_b ost
          WHERE     ose.stm_id = os.ID
                AND os.khr_id = p_contract_id
                AND os.say_code = 'CURR'
                AND os.active_yn = 'Y'
                AND os.sty_id = ost.ID
                AND ost.code = 'RENTAL ACCRUAL';

      --                AND TO_DATE (:p_period_name, 'Mon-YY');

*/
      lc_orig_product_name      VARCHAR2 (200);
      lcu_get_asset_costs_rec   get_asset_costs_c%ROWTYPE;
      --      lcu_get_asset_id_rec      get_asset_id_c%ROWTYPE;
      --      lcu_get_captl_rolvr_fee_rec   get_captl_rolvr_fee_c%ROWTYPE;
      lcu_get_cap_fee_rec       get_cap_fee_c%ROWTYPE;
      lcu_get_rolvr_fee_rec     get_rolvr_fee_c%ROWTYPE;
      lcu_get_addon_rec         get_addon_c%ROWTYPE;
      lcu_get_maintenance_rec   get_maintenance_c%ROWTYPE;
      lcu_get_warranty_rec      get_warranty_c%ROWTYPE;
      lcu_tmp_dtls_rec          get_tmp_dtls_c%ROWTYPE;
      ln_unit_cost              NUMBER;
      ln_asset_id               NUMBER;
      /*    ln_accum_asset_id         NUMBER;
          lc_book_type              VARCHAR2 (25);
          dummy_num                 NUMBER;
          ln_deprn_reserve          NUMBER;
          dummy_char                VARCHAR2 (200);
          dummy_bool                BOOLEAN;*/
      l_txd_id                  OKL_TXD_ASSETS_V.ID%TYPE;
      l_adpv_rec                OKL_TXD_ASSETS_PUB.adpv_rec_type;
      lx_adpv_rec               OKL_TXD_ASSETS_PUB.adpv_rec_type;
   BEGIN
      fnd_file.put_line (fnd_file.LOG,
                         'Getting the assets for the rebook contract');

      ln_tot_asst_cnt := 0;
      ln_tot_asst_cost := 0;
      ln_tot_slvg_val := 0;
      ln_tot_rol_fee := 0;
      ln_tot_cap_fee := 0;
      ln_tot_addon := 0;
      fnd_file.put_line (
         fnd_file.LOG,
         'the contract id for asset update is - ' || p_contract_id);

      FOR lcu_get_assets_rec IN get_assets_c (p_contract_id)
      LOOP
         ln_units := 0;
         ln_asset_cost := 0;
         ln_orig_cost := 0;
         ln_addon_fee := 0;
         ln_cap_rol_fee := 0;
         ln_cap_fee := 0;
         ln_rol_fee := 0;
         ln_asset_addon := 0;
         ln_unit_cost := 0;
         ln_asset_id := NULL;

         EXIT WHEN get_assets_c%NOTFOUND;

         ln_tot_asst_cnt := ln_tot_asst_cnt + 1;

         OPEN get_asset_units_c (p_contract_id, lcu_get_assets_rec.asset_name);

         FETCH get_asset_units_c INTO ln_units;

         CLOSE get_asset_units_c;

         fnd_file.put_line (
            fnd_file.LOG,
               'current units for asset '
            || lcu_get_assets_rec.asset_id
            || ' is - '
            || ln_units);

         fnd_file.put_line (fnd_file.LOG,
                            'Getting the asset cost and salvage value');

         OPEN get_asset_costs_c (lcu_get_assets_rec.asset_id);

         FETCH get_asset_costs_c INTO lcu_get_asset_costs_rec;

         OPEN get_asset_unit_cost (lcu_get_assets_rec.asset_id);

         FETCH get_asset_unit_cost INTO ln_unit_cost;

         --         OPEN get_captl_rolvr_fee_c (p_contract_id,
         --                                     lcu_get_assets_rec.asset_id);
         --
         --         FETCH get_captl_rolvr_fee_c INTO lcu_get_captl_rolvr_fee_rec;

         --         IF get_captl_rolvr_fee_c%NOTFOUND
         --         THEN
         --            ln_cap_rol_fee := 0;
         --         ELSE
         --            ln_cap_rol_fee := lcu_get_captl_rolvr_fee_rec.amount;
         --         END IF;

         OPEN get_cap_fee_c (p_contract_id, lcu_get_assets_rec.asset_id);

         FETCH get_cap_fee_c INTO lcu_get_cap_fee_rec;

         IF get_cap_fee_c%NOTFOUND
         THEN
            ln_cap_fee := 0;
         ELSE
            ln_cap_fee := lcu_get_cap_fee_rec.amount;
         END IF;

         OPEN get_rolvr_fee_c (p_contract_id, lcu_get_assets_rec.asset_id);

         FETCH get_rolvr_fee_c INTO lcu_get_rolvr_fee_rec;

         IF get_rolvr_fee_c%NOTFOUND
         THEN
            ln_rol_fee := 0;
         ELSE
            ln_rol_fee := lcu_get_rolvr_fee_rec.amount;
         END IF;


         fnd_file.put_line (
            fnd_file.LOG,
               'Capitalized fee for asset '
            || lcu_get_assets_rec.asset_name
            || ' is '
            || ln_cap_fee);

         fnd_file.put_line (
            fnd_file.LOG,
               'Rollover fee for asset '
            || lcu_get_assets_rec.asset_name
            || ' is '
            || ln_rol_fee);

         OPEN get_addon_c (p_contract_id, lcu_get_assets_rec.asset_id);

         FETCH get_addon_c INTO lcu_get_addon_rec;

         IF get_addon_c%NOTFOUND
         THEN
            ln_addon_fee := 0;
         ELSE
            ln_addon_fee := lcu_get_addon_rec.price;
         END IF;

         OPEN get_maintenance_c (p_contract_id, lcu_get_assets_rec.asset_id);

         FETCH get_maintenance_c INTO lcu_get_maintenance_rec;

         IF get_maintenance_c%NOTFOUND
         THEN
            ln_maintenance := 0;
         ELSE
            ln_maintenance := lcu_get_maintenance_rec.price;
         END IF;

         OPEN get_warranty_c (p_contract_id, lcu_get_assets_rec.asset_id);

         FETCH get_warranty_c INTO lcu_get_warranty_rec;

         IF get_warranty_c%NOTFOUND
         THEN
            ln_warranty := 0;
         ELSE
            ln_warranty := lcu_get_warranty_rec.price;
         END IF;


         fnd_file.put_line (
            fnd_file.LOG,
               'Addon fee for asset '
            || lcu_get_assets_rec.asset_id
            || ' is '
            || ln_addon_fee);

         fnd_file.put_line (
            fnd_file.LOG,
               'Maintenance for asset '
            || lcu_get_assets_rec.asset_id
            || ' is '
            || ln_maintenance);
         fnd_file.put_line (
            fnd_file.LOG,
               'Warranty for asset '
            || lcu_get_assets_rec.asset_id
            || ' is '
            || ln_warranty);

         fnd_file.put_line (
            fnd_file.LOG,
               'Number of Units for asset '
            || lcu_get_assets_rec.asset_id
            || ' is '
            || ln_units);
         fnd_file.put_line (
            fnd_file.LOG,
               'Units Cost of asset '
            || lcu_get_assets_rec.asset_id
            || ' is '
            || ln_unit_cost);
         ln_asset_cost := ln_units * (ln_unit_cost + ln_addon_fee);
         ln_asset_addon := ln_units * ln_addon_fee;
         fnd_file.put_line (
            fnd_file.LOG,
               'Depreciation cost of the asset '
            || lcu_get_assets_rec.asset_name
            || ' is '
            || ln_asset_cost);

         fnd_file.put_line (
            fnd_file.LOG,
               'Total addon * number of units for '
            || lcu_get_assets_rec.asset_name
            || ' is '
            || ln_asset_addon);

         --         ln_orig_cost := ln_asset_cost + ln_cap_rol_fee;
         ln_orig_cost := ln_asset_cost + ln_cap_fee + ln_rol_fee;
         fnd_file.put_line (
            fnd_file.LOG,
               'Original cost to be updated for the asset '
            || lcu_get_assets_rec.asset_name
            || ' is '
            || ln_orig_cost);

         fnd_file.put_line (fnd_file.LOG, 'Updating asset values');
         ln_asset_id := NULL;

         OPEN get_asset_id (p_rbk_chr_id, lcu_get_assets_rec.asset_name);

         FETCH get_asset_id INTO ln_asset_id;

         UPDATE okl_txl_assets_b
            SET depreciation_cost = ln_orig_cost,             --ln_asset_cost,
                original_cost = ln_orig_cost,
                salvage_value = lcu_get_asset_costs_rec.residual_value,
                depreciate_yn = 'Y',
                last_update_date = SYSDATE,
                last_updated_by = fnd_global.user_id,
                last_update_login = -1
          WHERE id = ln_asset_id;

         IF (SQL%ROWCOUNT = 0)
         THEN
            p_return_status := 'E';
         ELSE
            p_return_status := 'S';
         END IF;

         COMMIT;

         l_txd_id := NULL;

         OPEN get_asset_txd_id (ln_asset_id);

         FETCH get_asset_txd_id INTO l_txd_id;

         CLOSE get_asset_txd_id;


         OPEN get_orig_details (p_contract_id, p_request_id);

         FETCH get_orig_details INTO lc_orig_product_name;

         CLOSE get_orig_details;

         IF lc_orig_product_name LIKE '%NON%TAX%'
         THEN
            UPDATE okl_txd_assets_b
               SET cost = ln_orig_cost,
                   salvage_value = lcu_get_asset_costs_rec.residual_value,
                   last_update_date = SYSDATE,
                   last_updated_by = fnd_global.user_id,
                   last_update_login = fnd_global.user_id
             WHERE id = l_txd_id;

            IF (SQL%ROWCOUNT = 0)
            THEN
               p_return_status := 'E';
            ELSE
               p_return_status := 'S';
            END IF;

            COMMIT;
         END IF;

         ln_tot_asst_cost := ln_tot_asst_cost + ln_orig_cost;
         ln_tot_slvg_val :=
            ln_tot_slvg_val + lcu_get_asset_costs_rec.residual_value;
         ln_tot_cap_fee := ln_tot_cap_fee + ln_cap_fee;
         ln_tot_rol_fee := ln_tot_rol_fee + ln_rol_fee;
         ln_tot_addon := ln_tot_addon + ln_asset_addon;
         ln_tot_mntnc := ln_tot_mntnc + ln_maintenance;
         ln_tot_wrnty := ln_tot_wrnty + ln_warranty;

         CLOSE get_addon_c;

         CLOSE get_cap_fee_c;

         CLOSE get_rolvr_fee_c;

         CLOSE get_maintenance_c;

         CLOSE get_warranty_c;

         CLOSE get_asset_costs_c;

         CLOSE get_asset_unit_cost;

         CLOSE get_asset_id;
      END LOOP;


      --      OPEN get_rental_accrual_c (p_contract_id);
      --
      --      FETCH get_rental_accrual_c INTO ln_rental_accrual;
      --
      --      CLOSE get_rental_accrual_c;
      --
      --      OPEN get_rental_non_acc_c (p_contract_id);
      --
      --      FETCH get_rental_non_acc_c INTO ln_rental_non_accrual;
      --
      --      CLOSE get_rental_non_acc_c;
      --
      --      OPEN get_total_accrual_c (p_contract_id);
      --
      --      FETCH get_total_accrual_c INTO ln_tot_accrual;

      --      CLOSE get_total_accrual_c;


      ln_tot_incentive_amt :=
         ln_tot_cap_fee + ln_tot_rol_fee + ln_tot_mntnc + ln_tot_wrnty;
      fnd_file.put_line (fnd_file.LOG, 'ln_tot_asst_cnt ' || ln_tot_asst_cnt);
      fnd_file.put_line (fnd_file.LOG,
                         'ln_tot_asst_cost ' || ln_tot_asst_cost);
      fnd_file.put_line (fnd_file.LOG, 'ln_tot_slvg_val ' || ln_tot_slvg_val);
      fnd_file.put_line (fnd_file.LOG, 'ln_tot_cap_fee ' || ln_tot_cap_fee);
      fnd_file.put_line (fnd_file.LOG, 'ln_tot_rol_fee ' || ln_tot_rol_fee);
      fnd_file.put_line (fnd_file.LOG, 'ln_tot_addon ' || ln_tot_addon);
      fnd_file.put_line (fnd_file.LOG, 'ln_tot_mntnc ' || ln_tot_mntnc);
      fnd_file.put_line (fnd_file.LOG, 'ln_tot_wrnty ' || ln_tot_wrnty);
      fnd_file.put_line (fnd_file.LOG,
                         'ln_tot_incentive_amt ' || ln_tot_incentive_amt);

      --      fnd_file.put_line (fnd_file.LOG,
      --                         'ln_rental_accrual ' || ln_rental_accrual);
      --      fnd_file.put_line (fnd_file.LOG,
      --                         'ln_rental_non_accrual ' || ln_rental_non_accrual);
      --      fnd_file.put_line (fnd_file.LOG,
      --                         'The total accrual is ' || ln_tot_accrual);

      UPDATE xxd_ol_rbk_temp_t
         SET asset_cnt = ln_tot_asst_cnt,
             tot_asst_cost = ln_tot_asst_cost,
             tot_slvg_val = ln_tot_slvg_val,
             tot_cap_fee = ln_tot_cap_fee,
             tot_rol_fee = ln_tot_rol_fee,
             tot_add_on = ln_tot_addon,
             tot_res_val = ln_tot_slvg_val,             --same as residual val
             tot_maint = ln_tot_mntnc,
             tot_warranty = ln_tot_wrnty,
             incentive_amt = ln_tot_incentive_amt
       --             accrual_rent_inc = ln_rental_accrual,
       --             non_accrual_tot = ln_rental_non_accrual,
       --             total_accrual = ln_tot_accrual
       WHERE contract_id = p_contract_id AND request_id = p_request_id;

      COMMIT;
   EXCEPTION
      WHEN OTHERS
      THEN
         fnd_file.put_line (
            fnd_file.LOG,
            'Inside exception  - Update of asset values failed');
   END xxd_upd_asset_values;

   PROCEDURE xxd_upd_accum_deprn (p_contract_id        IN     NUMBER,
                                  p_request_id         IN     NUMBER,
                                  p_accum_deprn           OUT NUMBER,
                                  x_accum_ret_status      OUT VARCHAR2)
   IS
      ln_accum_asset_id    NUMBER;
      ln_accum_deprn       NUMBER;
      ln_tot_accum_deprn   NUMBER;
      lc_book_type         VARCHAR2 (25);
      dummy_num            NUMBER;
      ln_deprn_reserve     NUMBER;
      dummy_char           VARCHAR2 (200);
      dummy_bool           BOOLEAN;

      CURSOR get_accum_deprn_dtls (
         p_contract_id    NUMBER,
         p_asset_name     VARCHAR2)
      IS
         SELECT DISTINCT fab.asset_id, fbc.book_type_code
           FROM fa_deprn_detail fds,
                okc_k_lines_v a,
                okc_line_styles_b b,
                fa_book_controls fbc,
                fa_additions_b fab
          WHERE     1 = 1
                AND fds.asset_id = fab.asset_id
                AND fab.asset_number = a.name
                AND fab.asset_number = p_asset_name
                AND fds.book_type_code = fbc.book_type_code
                AND fbc.book_class = 'CORPORATE'
                AND a.dnz_chr_id = p_contract_id
                AND a.lse_id = b.id
                AND b.lty_code = 'FREE_FORM1';


      CURSOR get_assets_c (
         p_contract_id NUMBER)
      IS
         SELECT a.id asset_id, a.name asset_name
           FROM okc_k_lines_v a, okc_line_styles_b b
          WHERE     a.dnz_chr_id = p_contract_id
                AND a.lse_id = b.id                                       --33
                AND b.lty_code = 'FREE_FORM1'
                AND a.sts_code = 'BOOKED';
   BEGIN
      ln_tot_accum_deprn := 0;

      FOR lcu_get_assets_c_rec IN get_assets_c (p_contract_id)
      LOOP
         OPEN get_accum_deprn_dtls (p_contract_id,
                                    lcu_get_assets_c_rec.asset_name);

         FETCH get_accum_deprn_dtls
         INTO ln_accum_asset_id, lc_book_type;

         CLOSE get_accum_deprn_dtls;

         ln_deprn_reserve := 0;
         ln_accum_deprn := 0;

         fa_query_balances_pkg.query_balances (
            X_asset_id                => ln_accum_asset_id,
            X_book                    => lc_book_type,
            X_period_ctr              => 0,
            X_dist_id                 => 0,
            X_run_mode                => 'STANDARD',
            X_cost                    => dummy_num,
            X_deprn_rsv               => ln_deprn_reserve,
            X_reval_rsv               => dummy_num,
            X_ytd_deprn               => dummy_num,
            X_ytd_reval_exp           => dummy_num,
            X_reval_deprn_exp         => dummy_num,
            X_deprn_exp               => dummy_num,
            X_reval_amo               => dummy_num,
            X_prod                    => dummy_num,
            X_ytd_prod                => dummy_num,
            X_ltd_prod                => dummy_num,
            X_adj_cost                => dummy_num,
            X_reval_amo_basis         => dummy_num,
            X_bonus_rate              => dummy_num,
            X_deprn_source_code       => dummy_char,
            X_adjusted_flag           => dummy_bool,
            X_transaction_header_id   => -1,
            X_bonus_deprn_rsv         => dummy_num,
            X_bonus_ytd_deprn         => dummy_num,
            X_bonus_deprn_amount      => dummy_num,
            X_impairment_rsv          => dummy_num,
            X_ytd_impairment          => dummy_num,
            X_impairment_amount       => dummy_num);

         ln_accum_deprn := ln_accum_deprn + ln_deprn_reserve;
         ln_tot_accum_deprn := ln_tot_accum_deprn + ln_accum_deprn;
      END LOOP;

      --      UPDATE xxd_ol_rbk_temp_t
      --         SET accum_deprn = ln_tot_accum_deprn
      --       WHERE contract_id = p_contract_id AND request_id = p_request_id;
      --
      --      COMMIT;

      p_accum_deprn := ln_tot_accum_deprn;
      x_accum_ret_status := 'S';
   EXCEPTION
      WHEN OTHERS
      THEN
         x_accum_ret_status := 'E';
         fnd_file.put_line (
            fnd_file.LOG,
               'Error while updating accumulated depreciation value for contract - '
            || p_contract_id
            || SQLERRM);
   --         RETURN 'E';
   END xxd_upd_accum_deprn;

   PROCEDURE xxd_upd_rev_dtls_prc (p_contract_id   IN     NUMBER,
                                   p_request_id    IN     NUMBER,
                                   --                                   p_temp_dtls_rec  xxd_ol_rbk_temp_t%ROWTYPE,
                                   x_ret_status       OUT VARCHAR2)
   IS
      ln_unearn_leas_inc         NUMBER := 0;
      ln_acc_leas_inc            NUMBER := 0;
      ln_non_acc_leas_inc        NUMBER := 0;
      ln_unearn_res_inc          NUMBER := 0;
      ln_acc_res_inc             NUMBER := 0;
      ln_non_acc_res_inc         NUMBER := 0;
      ln_residual                NUMBER := 0;
      ln_broker_fee              NUMBER := 0;
      ln_acq_fee                 NUMBER := 0;
      ln_subsidy                 NUMBER := 0;
      ln_unbilled_rec            NUMBER := 0;
      ln_idc                     NUMBER := 0;

      CURSOR get_tmp_tbl_dtls_c (p_contract_id NUMBER, p_request_id NUMBER)
      IS
         SELECT *
           FROM xxd_ol_rbk_temp_t
          WHERE contract_id = p_contract_id AND request_id = p_request_id;


      CURSOR get_unearn_leas_inc_c (
         p_contract_id NUMBER)
      IS
         SELECT otac.amount
           FROM okl_ae_templates oet,
                okl_ae_tmpt_lnes oetl,
                okl_trns_acc_dstrs otac,
                okl_txl_cntrct_lns otcl,
                okl_trx_contracts otc,
                okl_formulae_b ofb
          WHERE     1 = 1
                AND oetl.avl_id = oet.id
                AND oetl.crd_code = otac.cr_dr_flag
                AND oetl.crd_code = 'D'
                AND otac.template_id = oet.id
                AND otc.id = otcl.tcn_id
                AND otcl.id = otac.source_id
                AND otac.source_table = 'OKL_TXL_CNTRCT_LNS'
                AND tcn_type = 'TRBK'
                AND rbr_code IS NULL
                AND otc.khr_id = p_contract_id
                AND ofb.id = oet.fma_id
                AND ofb.name = 'DLL_REV_DFL_UNEARNED_LEASE_INC';

      CURSOR get_acc_leas_inc_c (
         p_contract_id NUMBER)
      IS
         SELECT otac.amount
           FROM okl_ae_templates oet,
                okl_ae_tmpt_lnes oetl,
                okl_trns_acc_dstrs otac,
                okl_txl_cntrct_lns otcl,
                okl_trx_contracts otc,
                okl_formulae_b ofb
          WHERE     1 = 1
                AND oetl.avl_id = oet.id
                AND oetl.crd_code = otac.cr_dr_flag
                AND oetl.crd_code = 'D'
                AND otac.template_id = oet.id
                AND otc.id = otcl.tcn_id
                AND otcl.id = otac.source_id
                AND otac.source_table = 'OKL_TXL_CNTRCT_LNS'
                AND tcn_type = 'TRBK'
                AND rbr_code IS NULL
                AND otc.khr_id = p_contract_id
                AND ofb.id = oet.fma_id
                AND ofb.name = 'DLL_REV_DFL_ACCRUED_LEASE_INC';

      CURSOR get_non_acc_leas_inc_c (
         p_contract_id NUMBER)
      IS
         SELECT otac.amount
           FROM okl_ae_templates oet,
                okl_ae_tmpt_lnes oetl,
                okl_trns_acc_dstrs otac,
                okl_txl_cntrct_lns otcl,
                okl_trx_contracts otc,
                okl_formulae_b ofb
          WHERE     1 = 1
                AND oetl.avl_id = oet.id
                AND oetl.crd_code = otac.cr_dr_flag
                AND oetl.crd_code = 'D'
                AND otac.template_id = oet.id
                AND otc.id = otcl.tcn_id
                AND otcl.id = otac.source_id
                AND otac.source_table = 'OKL_TXL_CNTRCT_LNS'
                AND tcn_type = 'TRBK'
                AND rbr_code IS NULL
                AND otc.khr_id = p_contract_id
                AND ofb.id = oet.fma_id
                AND ofb.name = 'DLL_REV_DFL_NON_ACC_LEASE_INC';

      CURSOR get_unearn_res_inc_c (
         p_contract_id NUMBER)
      IS
         SELECT otac.amount
           FROM okl_ae_templates oet,
                okl_ae_tmpt_lnes oetl,
                okl_trns_acc_dstrs otac,
                okl_txl_cntrct_lns otcl,
                okl_trx_contracts otc,
                okl_formulae_b ofb
          WHERE     1 = 1
                AND oetl.avl_id = oet.id
                AND oetl.crd_code = otac.cr_dr_flag
                AND oetl.crd_code = 'D'
                AND otac.template_id = oet.id
                AND otc.id = otcl.tcn_id
                AND otcl.id = otac.source_id
                AND otac.source_table = 'OKL_TXL_CNTRCT_LNS'
                AND tcn_type = 'TRBK'
                AND rbr_code IS NULL
                AND otc.khr_id = p_contract_id
                AND ofb.id = oet.fma_id
                AND ofb.name = 'DLL_REV_DFL_UNEARNED_RES_INC';

      CURSOR get_acc_res_inc_c (
         p_contract_id NUMBER)
      IS
         SELECT otac.amount
           FROM okl_ae_templates oet,
                okl_ae_tmpt_lnes oetl,
                okl_trns_acc_dstrs otac,
                okl_txl_cntrct_lns otcl,
                okl_trx_contracts otc,
                okl_formulae_b ofb
          WHERE     1 = 1
                AND oetl.avl_id = oet.id
                AND oetl.crd_code = otac.cr_dr_flag
                AND oetl.crd_code = 'D'
                AND otac.template_id = oet.id
                AND otc.id = otcl.tcn_id
                AND otcl.id = otac.source_id
                AND otac.source_table = 'OKL_TXL_CNTRCT_LNS'
                AND tcn_type = 'TRBK'
                AND rbr_code IS NULL
                AND otc.khr_id = p_contract_id
                AND ofb.id = oet.fma_id
                AND ofb.name = 'DLL_REV_DFL_ACCRUED_RES_INC';

      CURSOR get_non_acc_res_inc_c (
         p_contract_id NUMBER)
      IS
         SELECT otac.amount
           FROM okl_ae_templates oet,
                okl_ae_tmpt_lnes oetl,
                okl_trns_acc_dstrs otac,
                okl_txl_cntrct_lns otcl,
                okl_trx_contracts otc,
                okl_formulae_b ofb
          WHERE     1 = 1
                AND oetl.avl_id = oet.id
                AND oetl.crd_code = otac.cr_dr_flag
                AND oetl.crd_code = 'D'
                AND otac.template_id = oet.id
                AND otc.id = otcl.tcn_id
                AND otcl.id = otac.source_id
                AND otac.source_table = 'OKL_TXL_CNTRCT_LNS'
                AND tcn_type = 'TRBK'
                AND rbr_code IS NULL
                AND otc.khr_id = p_contract_id
                AND ofb.id = oet.fma_id
                AND ofb.name = 'DLL_REV_DFL_NON_ACCRUAL_RES_INC';

      CURSOR get_unbilled_rec_c (
         p_contract_id NUMBER)
      IS
         SELECT otac.amount
           FROM okl_ae_templates oet,
                okl_ae_tmpt_lnes oetl,
                okl_trns_acc_dstrs otac,
                okl_txl_cntrct_lns otcl,
                okl_trx_contracts otc,
                okl_formulae_b ofb
          WHERE     1 = 1
                AND oetl.avl_id = oet.id
                AND oetl.crd_code = otac.cr_dr_flag
                AND oetl.crd_code = 'D'
                AND otac.template_id = oet.id
                AND otc.id = otcl.tcn_id
                AND otcl.id = otac.source_id
                AND otac.source_table = 'OKL_TXL_CNTRCT_LNS'
                AND tcn_type = 'TRBK'
                AND rbr_code IS NULL
                AND otc.khr_id = p_contract_id
                AND ofb.id = oet.fma_id
                AND ofb.name = 'DLL_REV_DFL_UNBILLED_RECEIVABLES';

      CURSOR get_residual_c (
         p_contract_id NUMBER)
      IS
         SELECT otac.amount
           FROM okl_ae_templates oet,
                okl_ae_tmpt_lnes oetl,
                okl_trns_acc_dstrs otac,
                okl_txl_cntrct_lns otcl,
                okl_trx_contracts otc,
                okl_formulae_b ofb
          WHERE     1 = 1
                AND oetl.avl_id = oet.id
                AND oetl.crd_code = otac.cr_dr_flag
                AND oetl.crd_code = 'D'
                AND otac.template_id = oet.id
                AND otc.id = otcl.tcn_id
                AND otcl.id = otac.source_id
                AND otac.source_table = 'OKL_TXL_CNTRCT_LNS'
                AND tcn_type = 'TRBK'
                AND rbr_code IS NULL
                AND otc.khr_id = p_contract_id
                AND ofb.id = oet.fma_id
                AND ofb.name = 'DLL_REV_DFL_RESIDUAL_VALUE';

      CURSOR get_broker_fee_c (
         p_contract_id NUMBER)
      IS
	--Start of modification by Priya on 06-Jan-2017
         /*SELECT otac.amount
           FROM okl_ae_templates oet,
                okl_ae_tmpt_lnes oetl,
                okl_trns_acc_dstrs otac,
                okl_txl_cntrct_lns otcl,
                okl_trx_contracts otc,
                okl_formulae_b ofb
          WHERE     1 = 1
                AND oetl.avl_id = oet.id
                AND oetl.crd_code = otac.cr_dr_flag
                AND oetl.crd_code = 'D'
                AND otac.template_id = oet.id
                AND otc.id = otcl.tcn_id
                AND otcl.id = otac.source_id
                AND otac.source_table = 'OKL_TXL_CNTRCT_LNS'
                AND tcn_type = 'TRBK'
                AND rbr_code IS NULL
                AND otc.khr_id = p_contract_id
                AND ofb.id = oet.fma_id
                AND ofb.name = 'DLL_REV_DFL_BROKER_FEE';*/
	SELECT otac.amount
	  FROM okl_ae_templates oet,
	       okl_ae_tmpt_lnes oetl,
	       okl_trns_acc_dstrs otac,
	       okl_txl_cntrct_lns otcl,
	       okl_trx_contracts otc,
	       okl_formulae_b ofb
	 WHERE 1 = 1
	   AND oetl.avl_id = oet.id
	   AND oetl.crd_code = otac.cr_dr_flag
	   AND oetl.crd_code = 'D'
	   AND otac.template_id = oet.id
	   AND otc.id = otcl.tcn_id
	   AND otcl.id = otac.source_id
	   AND otac.source_table = 'OKL_TXL_CNTRCT_LNS'
	   AND tcn_type = 'TRBK'
	   AND rbr_code IS NULL
	   AND otc.khr_id = p_contract_id
	   AND ofb.id = oet.fma_id
	   AND ofb.name IN ('DLL_REV_DFL_BROKER_FEE', 'DLL_BROKER_FEES')
	   AND otc.creation_date =
	              (SELECT MAX (creation_date)
	                 FROM okl_trx_contracts otc2
	                WHERE     otc2.tcn_type = 'TRBK'
	                      AND otc2.khr_id = otc.khr_id
        	              AND otc2.rbr_cOde IS NULL);
	--End of modification by Priya on 06-Jan-2017

      CURSOR get_acq_fee_c (
         p_contract_id NUMBER)
      IS
         SELECT otac.amount
           FROM okl_ae_templates oet,
                okl_ae_tmpt_lnes oetl,
                okl_trns_acc_dstrs otac,
                okl_txl_cntrct_lns otcl,
                okl_trx_contracts otc,
                okl_formulae_b ofb
          WHERE     1 = 1
                AND oetl.avl_id = oet.id
                AND oetl.crd_code = otac.cr_dr_flag
                AND oetl.crd_code = 'D'
                AND otac.template_id = oet.id
                AND otc.id = otcl.tcn_id
                AND otcl.id = otac.source_id
                AND otac.source_table = 'OKL_TXL_CNTRCT_LNS'
                AND tcn_type = 'TRBK'
                AND rbr_code IS NULL
                AND otc.khr_id = p_contract_id
                AND ofb.id = oet.fma_id
                AND ofb.name = 'DLL_REV_DFL_ACQ_FEE';

      CURSOR get_idc_c (
         p_contract_id NUMBER)
      IS
         SELECT otac.amount
           FROM okl_ae_templates oet,
                okl_ae_tmpt_lnes oetl,
                okl_trns_acc_dstrs otac,
                okl_txl_cntrct_lns otcl,
                okl_trx_contracts otc,
                okl_formulae_b ofb
          WHERE     1 = 1
                AND oetl.avl_id = oet.id
                AND oetl.crd_code = otac.cr_dr_flag
                AND oetl.crd_code = 'D'
                AND otac.template_id = oet.id
                AND otc.id = otcl.tcn_id
                AND otcl.id = otac.source_id
                AND otac.source_table = 'OKL_TXL_CNTRCT_LNS'
                AND tcn_type = 'TRBK'
                AND rbr_code IS NULL
                AND otc.khr_id = p_contract_id
                AND ofb.id = oet.fma_id
                AND ofb.name = 'DLL_REV_DFL_IDC';

      CURSOR get_subsidy_c (
         p_contract_id NUMBER)
      IS
         SELECT otac.amount
           FROM okl_ae_templates oet,
                okl_ae_tmpt_lnes oetl,
                okl_trns_acc_dstrs otac,
                okl_txl_cntrct_lns otcl,
                okl_trx_contracts otc,
                okl_formulae_b ofb
          WHERE     1 = 1
                AND oetl.avl_id = oet.id
                AND oetl.crd_code = otac.cr_dr_flag
                AND oetl.crd_code = 'D'
                AND otac.template_id = oet.id
                AND otc.id = otcl.tcn_id
                AND otcl.id = otac.source_id
                AND otac.source_table = 'OKL_TXL_CNTRCT_LNS'
                AND tcn_type = 'TRBK'
                AND rbr_code IS NULL
                AND otc.khr_id = p_contract_id
                AND ofb.id = oet.fma_id
                AND ofb.name = 'DLL_REV_DFL_SUBSIDY';

      lcu_get_tmp_tbl_dtls_rec   get_tmp_tbl_dtls_c%ROWTYPE;
   BEGIN
      fnd_file.put_line (
         fnd_file.LOG,
            ' Fetching the reversal accounting entries for the contract-id '
         || p_contract_id);

      OPEN get_tmp_tbl_dtls_c (p_contract_id, p_request_id);

      FETCH get_tmp_tbl_dtls_c INTO lcu_get_tmp_tbl_dtls_rec;

      CLOSE get_tmp_tbl_dtls_c;

      OPEN get_unearn_leas_inc_c (p_contract_id);

      FETCH get_unearn_leas_inc_c INTO ln_unearn_leas_inc;

      CLOSE get_unearn_leas_inc_c;

      OPEN get_acc_leas_inc_c (p_contract_id);

      FETCH get_acc_leas_inc_c INTO ln_acc_leas_inc;

      CLOSE get_acc_leas_inc_c;

      OPEN get_non_acc_leas_inc_c (p_contract_id);

      FETCH get_non_acc_leas_inc_c INTO ln_non_acc_leas_inc;

      CLOSE get_non_acc_leas_inc_c;

      OPEN get_unearn_res_inc_c (p_contract_id);

      FETCH get_unearn_res_inc_c INTO ln_unearn_res_inc;

      CLOSE get_unearn_res_inc_c;

      OPEN get_acc_res_inc_c (p_contract_id);

      FETCH get_acc_res_inc_c INTO ln_acc_res_inc;

      CLOSE get_acc_res_inc_c;

      OPEN get_non_acc_res_inc_c (p_contract_id);

      FETCH get_non_acc_res_inc_c INTO ln_non_acc_res_inc;

      CLOSE get_non_acc_res_inc_c;

      OPEN get_unbilled_rec_c (p_contract_id);

      FETCH get_unbilled_rec_c INTO ln_unbilled_rec;

      CLOSE get_unbilled_rec_c;

      OPEN get_residual_c (p_contract_id);

      FETCH get_residual_c INTO ln_residual;

      CLOSE get_residual_c;

      OPEN get_broker_fee_c (p_contract_id);

      FETCH get_broker_fee_c INTO ln_broker_fee;

      CLOSE get_broker_fee_c;

      OPEN get_acq_fee_c (p_contract_id);

      FETCH get_acq_fee_c INTO ln_acq_fee;

      CLOSE get_acq_fee_c;

      OPEN get_idc_c (p_contract_id);

      FETCH get_idc_c INTO ln_idc;

      CLOSE get_idc_c;

      OPEN get_subsidy_c (p_contract_id);

      FETCH get_subsidy_c INTO ln_subsidy;

      CLOSE get_subsidy_c;

      fnd_file.put_line (fnd_file.LOG,
                         'Updating the temp table accounting columns');

      --      lcu_get_tmp_tbl_dtls_rec := p_temp_dtls_rec;
      lcu_get_tmp_tbl_dtls_rec.REV_DFL_UN_LEASE_INC :=
         NVL (ln_unearn_leas_inc, 0);
      lcu_get_tmp_tbl_dtls_rec.REV_DFL_ACC_LEASE_INC :=
         NVL (ln_acc_leas_inc, 0);
      lcu_get_tmp_tbl_dtls_rec.REV_DFL_NAC_LEASE_INC :=
         NVL (ln_non_acc_leas_inc, 0);
      lcu_get_tmp_tbl_dtls_rec.REV_DFL_UN_RES_INC :=
         NVL (ln_unearn_res_inc, 0);
      lcu_get_tmp_tbl_dtls_rec.REV_DFL_ACC_RES_INC := NVL (ln_acc_res_inc, 0);
      lcu_get_tmp_tbl_dtls_rec.REV_DFL_NAC_RES_INC :=
         NVL (ln_non_acc_res_inc, 0);
      lcu_get_tmp_tbl_dtls_rec.REV_DFL_UNBLD_REC := NVL (ln_unbilled_rec, 0);
      lcu_get_tmp_tbl_dtls_rec.REV_DFL_BROKER_FEE := NVL (ln_broker_fee, 0);
      lcu_get_tmp_tbl_dtls_rec.REV_DFL_ACQ_FEE := NVL (ln_acq_fee, 0);
      lcu_get_tmp_tbl_dtls_rec.REV_DFL_SUBSIDY := NVL (ln_subsidy, 0);
      lcu_get_tmp_tbl_dtls_rec.REV_DFL_IDC := NVL (ln_idc, 0);
      lcu_get_tmp_tbl_dtls_rec.REV_DFL_RESIDUAL := NVL (ln_residual, 0);

      xxd_upd_temp_tab_proc (lcu_get_tmp_tbl_dtls_rec);
   EXCEPTION
      WHEN OTHERS
      THEN
         fnd_file.put_line (
            fnd_file.LOG,
            'The reversal values update went into an unexpected exception');
         x_ret_status := 'E';
   END xxd_upd_rev_dtls_prc;

   /*=============================================================================+
     | PROCEDURE: OL_RBK_MASTER_PROC
     |
     | DESCRIPTION: This PL/SQL procedure is the master program which fetches eligible records and submits child processes
     |
     | PARAMETERS:
     | IN:      p_number_of_process
     |OUT:
     |       retcode  : Returns status of the procedure
     |       errbuf : Return the error message
     |
     | SCOPE:     PUBLIC procedure
     |
     | EXTERNAL PROCEDURES/FUNCTIONS USED:
     |   OWNER              NAME                                TYPE
     |   ---------------    ------------------------------      -----------
     |   NA
     |
     | HISTORY:
     |  WHO               WHEN                    WHAT
     |  --------------    ---------------         -------------------------------
     |  Sherin           1o-May-2016            Initial Version
     |
     +============================================================================*/

   PROCEDURE ol_rbk_master_proc (
      errbuf                   OUT VARCHAR2,
      retcode                  OUT VARCHAR2,
      p_set_of_books_id     IN     NUMBER,
      p_operating_unit_id   IN     NUMBER,
      p_component_code      IN     VARCHAR2,
      p_stage_code          IN     VARCHAR2,
      p_country_code        IN     VARCHAR2,
      p_run_type            IN     VARCHAR2,
      p_cont_run_type       IN     VARCHAR2 DEFAULT NULL,
      p_file_run_type       IN     VARCHAR2 DEFAULT NULL,
      p_int_dir             IN     VARCHAR2,
      p_filename            IN     VARCHAR2,
      p_contract_number     IN     VARCHAR2,
      p_number_of_process   IN     NUMBER DEFAULT 20,
      p_time_out            IN     NUMBER DEFAULT 40 --      p_reprocess_flag IN VARCHAR2 DEFAULT 'N'
                                                    )
   IS
      --This cursor is used to fetch when individual contract number is passed
      CURSOR get_contract_c (
         p_contract_number VARCHAR2)
      IS
         SELECT hdr.id contract_id,
                hdr.contract_number,
                hdr.authoring_org_id org_id,
                start_date,
                end_date,
                reporting_entity entity,
                hdr.sts_code sts_code,
                'OPERATING' class,
                okl.pdt_id orig_pdt_id
           FROM apps.okc_k_headers_b hdr,
                xxd_okl_k_headers_t xxd,
                okl_k_headers okl
          WHERE     hdr.scs_code = 'LEASE'
                AND hdr.sts_code = 'BOOKED'
                AND hdr.authoring_org_id = gn_org_id
                AND hdr.id = xxd.khr_id
                AND okl.id = hdr.id
                AND hdr.contract_number = p_contract_number;

      --                AND NOT EXISTS
      --                           (SELECT 1
      --                              FROM xxd_ol_rbk_temp_t
      --                             WHERE     contract_number = p_contract_number
      --                                   AND processed_flag = 'Y');

      CURSOR get_cont_dtls_c (
         p_contract_number    VARCHAR2,
         p_conc_req_id        NUMBER)
      IS
         SELECT xxd.contract_id,
                xxd.contract_number,
                xxd.org_id,
                xxd.start_date,
                xxd.end_date,
                xxd.entity,
                xxd.sts_code,
                xxd.class,
                xxd.orig_pdt_id,
                xxd.status,
                xxd.processed_flag,
                xxd.error_msg
           FROM xxd_ol_rbk_temp_t xxd
          WHERE     xxd.contract_number = p_contract_number
                AND request_id = p_conc_req_Id
                AND xxd.run_type = 'CONTRACT'
                AND error_msg IS NULL;

      -- This cursor is for when run_type is selected as FILE
      CURSOR get_contracts_c (
         p_number_of_process NUMBER)
      IS
         SELECT tmp.batch_date,
                hdr.id contract_id,
                hdr.contract_number,
                hdr.authoring_org_id org_id,
                hdr.start_date,
                hdr.end_date,
                xxd.reporting_entity entity,
                hdr.sts_code sts_code,
                'OPERATING' class,
                okl.pdt_id orig_pdt_id,
                NTILE (p_number_of_process)
                   OVER (ORDER BY tmp.contract_number)
                   AS child_number,
                DECODE (tmp.comments, 'NEW', 'P', NULL) status,
                DECODE (tmp.comments, 'NEW', 'N', NULL) processed_flag,
                DECODE (tmp.comments, 'NEW', NULL, tmp.comments) comments
           FROM apps.okc_k_headers_b hdr,
                xxd_okl_k_headers_t xxd,
                okl_k_headers okl,
                xxd_ol_data_load_t tmp
          WHERE tmp.contract_number = hdr.contract_number --Commented out on 14th September to make sure other OU contarcts are recorded in temp table
                                                          --                AND hdr.authoring_org_id = gn_org_id
                AND okl.id = hdr.id AND hdr.id = xxd.khr_id
         --Commented out on 14th September to make sure Evg/Terminated contracts are recorded in temp table
         --                AND hdr.sts_code = 'BOOKED'
         --                AND hdr.scs_code = 'LEASE'
         --                AND tmp.comments = 'NEW'                       -- to include all loader comments 7th sep
         --                AND NOT EXISTS
         --                           (SELECT 1
         --                              FROM xxd_ol_rbk_temp_t temp
         --                             WHERE     temp.contract_number =
         --                                          tmp.contract_number
         --                                   AND processed_flag = 'Y')
         UNION ALL
         SELECT tmp.batch_date,
                0 contract_id,
                tmp.contract_number,
                NULL org_id,
                NULL start_date,
                NULL end_date,
                NULL entity,
                NULL sts_code,
                'OPERATING' class,
                NULL orig_pdt_id,
                NTILE (p_number_of_process)
                   OVER (ORDER BY tmp.contract_number)
                   AS child_number,
                DECODE (tmp.comments, 'NEW', 'P', NULL) status,
                DECODE (tmp.comments, 'NEW', 'N', NULL) processed_flag,
                'Contract not available to process' comments
           FROM xxd_ol_data_load_t tmp
          WHERE     1 = 1
                AND NOT EXISTS
                           (SELECT 1
                              FROM okc_k_headers_b hdr
                             WHERE     1 = 1
                                   AND tmp.contract_number =
                                          hdr.contract_number);

      CURSOR get_repr_cont_c (
         p_number_of_process NUMBER)
      IS
         SELECT tmp.contract_number,
                tmp.contract_id,
                tmp.request_id,
                tmp.status,
                NTILE (p_number_of_process)
                   OVER (ORDER BY tmp.contract_number)
                   AS child_num
           FROM xxd_ol_rbk_temp_t tmp
          WHERE     tmp.processed_flag = 'N'
                AND error_msg NOT IN
                       ('Rebook Request Exists',
                        'Termination Quote Exists',
                        'Contract not available to process',
                        'This contract has an Evergreen Rent billed stream') --excluding req contracts for reprocessing
                AND run_type = 'FILE'
                AND request_id =
                       (SELECT MAX (request_id)    --latest unprocessed record
                          FROM xxd_ol_rbk_temp_t t
                         WHERE     t.contract_number = tmp.contract_number
                               AND processed_flag = 'N')
                AND NOT EXISTS
                           (SELECT 1
                              FROM xxd_ol_rbk_temp_t temp
                             WHERE     temp.contract_number =
                                          tmp.contract_number
                                   AND processed_flag = 'Y');

      CURSOR get_rep_cont_c         -- this is to validate rebook/qte requests
      IS
         SELECT tmp.contract_number,
                tmp.contract_id,
                tmp.request_id,
                tmp.status,
                tmp.child_num,
                tmp.error_msg
           FROM xxd_ol_rbk_temp_t tmp
          WHERE     tmp.processed_flag = 'N'
                AND run_type = 'FILE'
                AND request_id =
                       (SELECT MAX (request_id)    --latest unprocessed record
                          FROM xxd_ol_rbk_temp_t t
                         WHERE     t.contract_number = tmp.contract_number
                               AND processed_flag = 'N')
                AND NOT EXISTS
                           (SELECT 1
                              FROM xxd_ol_rbk_temp_t temp
                             WHERE     temp.contract_number =
                                          tmp.contract_number
                                   AND processed_flag = 'Y');

      CURSOR get_exs_contracts
      IS
         SELECT contract_number
           FROM xxd_ol_data_load_t tab
          WHERE     TRUNC (batch_date) = TRUNC (SYSDATE)
                AND EXISTS
                       (SELECT 1
                          FROM xxd_ol_rbk_temp_t temp
                         WHERE     tab.contract_number = temp.contract_number
                               AND processed_flag = 'Y');

      CURSOR get_unp_cnt_c
      IS
         SELECT COUNT (DISTINCT contract_number)
           FROM xxd_ol_rbk_temp_t tab
          WHERE     TRUNC (batch_date) = TRUNC (SYSDATE)
                AND run_type = 'FILE'
                AND error_msg IN
                       ('This contract has already been reclassified to Operating Lease',
                        'Termination Quote Exists',
                        'Rebook Request Exists',
                        'This contract is of a different operating unit than the current run',
                        'This contract is not in BOOKED status',
                        --added to pick junk contracts as well
                        'Contract not available to process',
                        'This contract has an Evergreen Rent billed stream');

      CURSOR get_unp_c
      IS
         SELECT COUNT (DISTINCT contract_number)
           FROM xxd_ol_rbk_temp_t tab
          WHERE     TRUNC (batch_date) = TRUNC (SYSDATE)
                AND run_type = 'CONTRACT'
                AND error_msg IN
                       ('This contract has already been reclassified to Operating Lease',
                        'Termination Quote Exists',
                        'Rebook Request Exists',
                        'This contract is of a different operating unit than the current run',
                        'This contract is not in BOOKED status',
                        'This contract has an Evergreen Rent billed stream');

      CURSOR get_pend_rbk_c
      IS
         SELECT COUNT (DISTINCT contract_id)
           FROM xxd_ol_rbk_temp_t temp
          WHERE     1 = 1
                --          AND TRUNC (batch_date) = TRUNC (SYSDATE)
                AND error_msg = 'Rebook Request Exists'
                AND processed_flag = 'N'
                AND EXISTS
                       (SELECT 1
                          FROM okl_trx_requests otr
                         WHERE     1 = 1
                               AND otr.dnz_khr_id = temp.contract_id
                               AND request_type_code = 'DLL_PROPOSED_RB_REQ'
                               AND otr.request_status_code = 'APPROVED')
                AND NOT EXISTS
                           (SELECT 1
                              FROM xxd.xxd_ol_rbk_temp_T a
                             WHERE     status = 'S'
                                   AND a.contract_number =
                                          temp.contract_number);

      CURSOR get_pend_qte_c
      IS
         SELECT COUNT (DISTINCT contract_id)
           FROM xxd_ol_rbk_temp_t temp
          WHERE     1 = 1
                --          AND TRUNC (batch_date) = TRUNC (SYSDATE)
                AND error_msg = 'Termination Quote Exists'
                AND processed_flag = 'N'
                AND EXISTS
                       (SELECT 1
                          FROM okl_trx_quotes_b qte
                         WHERE     qst_code IN ('APPROVED', 'ACCEPTED')
                               AND date_effective_to >= SYSDATE
                               AND qte.khr_id = temp.contract_id)
                AND NOT EXISTS
                           (SELECT 1
                              FROM xxd.xxd_ol_rbk_temp_T a
                             WHERE     status = 'S'
                                   AND a.contract_number =
                                          temp.contract_number);

      CURSOR get_evg_contracts
      IS
         SELECT tab.contract_number
           FROM xxd_ol_data_load_t tab, okc_k_headers_b hdr
          WHERE     tab.contract_number = hdr.contract_number
                AND TRUNC (batch_date) = TRUNC (SYSDATE)
                AND hdr.sts_code <> 'BOOKED';

      CURSOR get_pmc_contracts
      IS
         SELECT a.contract_number
           FROM xxd_ol_data_load_t a, okc_k_headers_b b
          WHERE     a.contract_number = b.contract_number
                AND b.authoring_org_id <> gn_org_id;

      CURSOR get_exs_contract (
         p_contract_number VARCHAR2)
      IS
         SELECT DISTINCT temp.contract_number
           FROM xxd_ol_rbk_temp_t temp
          WHERE     temp.contract_number = p_contract_number
                AND EXISTS
                       (SELECT 1
                          FROM xxd_ol_rbk_temp_t tab
                         WHERE     tab.contract_number = temp.contract_number
                               AND tab.processed_flag = 'Y');

      CURSOR get_evg_billed_cont_c
      IS
         SELECT tmp.contract_number
           FROM xxd_ol_data_load_t tmp, okc_k_headers_b a
          WHERE     tmp.contract_number = a.contract_number
                AND EXISTS
                       (SELECT 1
                          FROM okl_streams b,
                               okl_strm_type_b c,
                               okl_strm_elements d
                         WHERE     a.id = b.khr_id
                               AND b.sty_id = c.id
                               AND d.stm_id = b.id
                               AND c.code IN
                                      ('EVERGREEN RENT', 'RENT EVERGREEN')
                               AND b.say_code = 'CURR'
                               AND b.active_yn = 'Y'
                               AND d.date_billed IS NOT NULL);

      CURSOR get_evg_billd_c (
         p_contract_number VARCHAR2)
      IS
         SELECT DISTINCT 1
           FROM okc_k_headers_b a,
                okl_streams b,
                okl_strm_type_b c,
                okl_strm_elements d
          WHERE     a.contract_number = p_contract_number
                AND b.khr_id = a.id
                AND b.sty_id = c.id
                AND d.stm_id = b.id
                AND c.code IN ('EVERGREEN RENT', 'RENT EVERGREEN')
                AND b.say_code = 'CURR'
                AND b.active_yn = 'Y'
                AND d.date_billed IS NOT NULL;

      CURSOR get_pdt_id_c (
         p_contract_id2 NUMBER)
      IS
         SELECT op.id orig_product_id, op_new.id new_product_id, op.name
           FROM okl_products op,
                okc_k_headers_b okc,
                okl_k_headers okh,
                okl_products op_new
          WHERE     op.id = okh.pdt_id
                AND okc.id = okh.id
                AND okc.id = p_contract_id2
                AND okc.authoring_org_id = gn_org_id
                AND op_new.name = gd_pdt_name;

      -- if it is for a file passed, we will take the data from the loader table

      --      CURSOR get_file_cnt_c
      --      IS
      --         SELECT COUNT (DISTINCT contract_id)
      --           FROM XXD_OL_RBK_TEMP_T tmp
      --          WHERE     TRUNC (BATCH_DATE) = TRUNC (SYSDATE)
      --                AND status <> 'E'
      --                AND processed_flag = 'N'
      --                AND NOT EXISTS
      --                           (SELECT 1
      --                              FROM xxd_ol_data_load_t a
      --                             WHERE     a.contract_number =
      --                                          tmp.contract_number
      --                                   AND TRUNC (a.batch_date) =
      --                                          TRUNC (tmp.batch_date)
      --                                   AND comments =
      --                                          'This contract has already been Reclassified');

      CURSOR get_temp_count_c
      IS
         SELECT COUNT (contract_id)
           FROM XXD_OL_RBK_TEMP_T
          WHERE     TRUNC (BATCH_DATE) = TRUNC (SYSDATE)
                AND processed_flag = 'N'
                AND status <> 'E';

      CURSOR get_parent_sts_c (
         cp_request_id NUMBER)
      IS
         SELECT COUNT (1)
           FROM fnd_concurrent_requests req, fnd_concurrent_programs_vl pgm
          WHERE     req.priority_request_id = cp_request_id
                AND req.concurrent_program_id = pgm.concurrent_program_id
                AND req.phase_code = 'C'
                AND request_id <> cp_request_id
                AND status_code = 'E';

      CURSOR get_parent_warn_sts_c (
         cp_request_id NUMBER)
      IS
         SELECT COUNT (1)
           FROM fnd_concurrent_requests req, fnd_concurrent_programs_vl pgm
          WHERE     req.priority_request_id = cp_request_id
                AND req.concurrent_program_id = pgm.concurrent_program_id
                AND req.phase_code = 'C'
                AND request_id <> cp_request_id
                AND status_code = 'G';


      TYPE req_rec IS RECORD (reqid NUMBER);

      TYPE req_id_tab IS TABLE OF req_rec
                            INDEX BY BINARY_INTEGER;

      --
      -- variables
      --
      l_canc_ret_sts                VARCHAR2 (1);
      l_qte_ret_sts                 VARCHAR2 (1);
      ln_child_in_error             NUMBER;
      ln_child_in_warn              NUMBER;
      lc_req_data                   VARCHAR2 (10);
      ln_request_id                 NUMBER;
      ln_loader_req_id              NUMBER;
      ln_temp_count                 NUMBER := 0;
      ln_pdt_id                     okl_products.id%TYPE;
      lc_ret_sts                    VARCHAR2 (100);
      ln_batch_id                   VARCHAR2 (100);
      lcu_get_exs_contract          get_exs_contract%ROWTYPE;
      lcu_get_evg_billed_rec        get_evg_billd_c%ROWTYPE;
      ex_load_processing            EXCEPTION;
      --      ln_orig_pdt_id           okl_products.id%TYPE;
      --      lc_product_name     okl_products.name%TYPE;
      INTERVAL             CONSTANT NUMBER := 1;
      --     1 seconds before checking if request is finished

      max_wait             CONSTANT NUMBER := 30;
      --       wait cancelling the submitted request wait

      v_request_id                  fnd_concurrent_requests.request_id%TYPE;
      v_return_status               NUMBER;
      v_error_message               VARCHAR2 (4000);

      id_seq                        NUMBER := 0;
      id_seq1                       NUMBER := 0;
      req_id_tbl                    req_id_tab;

      wait_for_req                  BOOLEAN;

      lc_return_status              VARCHAR2 (100);
      lc_error_message              VARCHAR2 (2000);

      --      lb_req_completed      BOOLEAN;
      --
      --      --v_statement         VARCHAR2 (4000);
      v_phase                       VARCHAR2 (200);    -- out par wait request
      v_status                      VARCHAR2 (200);    -- out par wait request
      v_dev_phase                   VARCHAR2 (200);    -- out par wait request
      v_dev_status                  VARCHAR2 (200);    -- out par wait request
      v_message                     VARCHAR2 (4000);

      l_trans_rec                   fa_api_types.trans_rec_type;
      l_asset_hdr_rec               fa_api_types.asset_hdr_rec_type;
      l_asset_fin_rec_adj           fa_api_types.asset_fin_rec_type;
      l_asset_fin_rec_new           fa_api_types.asset_fin_rec_type;
      l_asset_fin_mrc_tbl_new       fa_api_types.asset_fin_tbl_type;
      l_inv_trans_rec               fa_api_types.inv_trans_rec_type;
      l_inv_tbl                     fa_api_types.inv_tbl_type;
      l_inv_rate_tbl                fa_api_types.inv_rate_tbl_type;
      l_asset_deprn_rec_adj         fa_api_types.asset_deprn_rec_type;
      l_asset_deprn_rec_new         fa_api_types.asset_deprn_rec_type;
      l_asset_deprn_mrc_tbl_new     fa_api_types.asset_deprn_tbl_type;
      l_inv_rec                     fa_api_types.inv_rec_type;
      l_group_reclass_options_rec   fa_api_types.group_reclass_options_rec_type;
      l_return_status2              VARCHAR2 (1);
      l_mesg_count                  NUMBER := 0;
      l_mesg_len                    NUMBER;
      l_mesg                        VARCHAR2 (4000);
      lc_return_status2             VARCHAR2 (1);

      p_reprocess_flag              VARCHAR2 (1);
      lc_rbk_flag                   VARCHAR2 (1);
      lc_qte_flag                   VARCHAR2 (1);

      --Start of modification by Priya for Sending mail
      CURSOR file_name_c (
         p_conc_prog_name VARCHAR2)
      IS
         SELECT SUBSTR (NVL (cro.file_name, cor.outfile_name),
                          INSTR (NVL (cro.file_name, cor.outfile_name),
                                 '/',
                                 -1,
                                 1)
                        + 1)
                   file_name
           FROM fnd_conc_req_outputs cro,
                fnd_concurrent_requests cor,
                fnd_concurrent_programs cp
          WHERE     cro.concurrent_request_id(+) = cor.request_id
                AND cor.parent_request_id = fnd_global.conc_request_id
                AND cp.concurrent_program_id = cor.concurrent_program_id
                AND cp.concurrent_program_name = p_conc_prog_name;

      CURSOR c_rbk_get_to_address
      IS
         SELECT attribute1
           FROM fnd_lookup_values
          WHERE     lookup_type = 'DLL_OPL_MAIL_NOTIFICATION'
                AND lookup_code = 'DLL_RBK_REQ_EMAIL'
                AND LANGUAGE = 'US'
                AND SYSDATE BETWEEN start_date_active
                                AND NVL (end_date_active, SYSDATE + 1);

      CURSOR c_termn_get_to_address
      IS
         SELECT attribute1
           FROM fnd_lookup_values
          WHERE     lookup_type = 'DLL_OPL_MAIL_NOTIFICATION'
                AND lookup_code = 'DLL_TRMN_QTE_EMAIL'
                AND LANGUAGE = 'US'
                AND SYSDATE BETWEEN start_date_active
                                AND NVL (end_date_active, SYSDATE + 1);


      lc_extract                    VARCHAR2 (31000);

      lc_error_hdr                  CLOB := EMPTY_CLOB ();
      lc_error_dtl                  CLOB := EMPTY_CLOB ();
      lc_error_dtl_tmp              VARCHAR2 (32000);

      lc_smpt_host                  VARCHAR2 (100);
      lc_smpt_port                  VARCHAR2 (100);
      lc_to                         VARCHAR2 (100);
      l_xxd_eci_attach_tbl          xxd_okl_eci_inv_x_pk.l_email_attach;
      ln_request_id2                NUMBER;
      ln_request_id3                NUMBER;
      lbf_xls_file                  BFILE;
      lbb_xls_file                  BLOB;
      lbfq_xls_file                 BFILE;
      lbbq_xls_file                 BLOB;
      lc_rename_file_name           VARCHAR2 (100);
      lc_file_name                  VARCHAR2 (100);
      lb_request_layout             BOOLEAN;
      lb_qte_req_layout             BOOLEAN;
      lc_req_data2                  VARCHAR2 (10);
   --End of modification by Priya for sending mail


   BEGIN
      gn_conc_request_id := fnd_global.conc_request_id;

      lc_req_data := fnd_conc_global.request_data;

      ln_rbk_cnt := 0;

      ln_qte_cnt := 0;

      IF lc_req_data = 4
      THEN
         OPEN get_pend_rbk_c;

         FETCH get_pend_rbk_c INTO ln_rbk_cnt;

         CLOSE get_pend_rbk_c;

         OPEN get_pend_qte_c;

         FETCH get_pend_qte_c INTO ln_qte_cnt;

         CLOSE get_pend_qte_c;

         IF ln_rbk_cnt > 0
         THEN
            OPEN c_rbk_get_to_address;

            FETCH c_rbk_get_to_address INTO lc_to;

            CLOSE c_rbk_get_to_address;

            OPEN file_name_c ('XXDOPLRBKREQ');

            FETCH file_name_c INTO lc_file_name;

            CLOSE file_name_c;


            lc_rename_file_name :=
                  'DLL_OPL_Approved_Rebook_Req_'
               || fnd_global.conc_request_id
               || '.xls';

            lbf_xls_file :=
               BFILENAME ('XXD_GLOBAL_CONC_OUT_DIR', lc_file_name);

            DBMS_LOB.OPEN (lbf_xls_file, DBMS_LOB.lob_readonly);
            DBMS_LOB.createtemporary (lbb_xls_file, FALSE);
            DBMS_LOB.OPEN (lbb_xls_file, DBMS_LOB.lob_readwrite);
            DBMS_LOB.LOADFROMFILE (
               dest_lob   => lbb_xls_file,
               src_lob    => lbf_xls_file,
               amount     => DBMS_LOB.getLength (lbf_xls_file));

            DBMS_LOB.CLOSE (lbf_xls_file);
            l_xxd_eci_attach_tbl (1).attach_name := lc_rename_file_name;
            l_xxd_eci_attach_tbl (1).attach_mime := 'application/vnd.ms-excel';
            DBMS_LOB.createtemporary (l_xxd_eci_attach_tbl (1).attach_blob,
                                      FALSE);
            DBMS_LOB.OPEN (l_xxd_eci_attach_tbl (1).attach_blob,
                           DBMS_LOB.lob_readwrite);
            DBMS_LOB.COPY (dest_lob      => l_xxd_eci_attach_tbl (1).attach_blob,
                           src_lob       => lbb_xls_file,
                           amount        => DBMS_LOB.getLength (lbb_xls_file),
                           dest_offset   => 1,
                           src_offset    => 1);

            lc_smpt_host := fnd_profile.VALUE ('XXD_LSS_OUTBOUND_MAIL_SERVER');
            lc_smpt_port := 25;

            fnd_file.put_line (fnd_file.LOG,
                               'Calling send_email_notification');

            xxd_okl_eci_inv_x_pk.send_email_notification (
               p_to                   => lc_to,
               p_from                 => 'noreply@delagelanden.com',
               p_subject              => 'DLL Operating Lease Rebook Request Report',
               p_text_msg             => NULL,
               p_xxd_eci_attach_tbl   => l_xxd_eci_attach_tbl,
               p_smtp_host            => lc_smpt_host,
               p_smtp_port            => lc_smpt_port);
         END IF;


         IF ln_qte_cnt > 0
         THEN
            lc_file_name := NULL;
            lc_rename_file_name := NULL;

            OPEN c_termn_get_to_address;

            FETCH c_termn_get_to_address INTO lc_to;

            CLOSE c_termn_get_to_address;


            OPEN file_name_c ('XXDOPLTERMQTE');

            FETCH file_name_c INTO lc_file_name;

            CLOSE file_name_c;

            lc_rename_file_name :=
                  'DLL_OPL_Pending_Terminations_'
               || fnd_global.conc_request_id
               || '.xls';
            lbfq_xls_file :=
               BFILENAME ('XXD_GLOBAL_CONC_OUT_DIR', lc_file_name);

            DBMS_LOB.OPEN (lbfq_xls_file, DBMS_LOB.lob_readonly);
            DBMS_LOB.createtemporary (lbbq_xls_file, FALSE);
            DBMS_LOB.OPEN (lbbq_xls_file, DBMS_LOB.lob_readwrite);
            DBMS_LOB.LOADFROMFILE (
               dest_lob   => lbbq_xls_file,
               src_lob    => lbfq_xls_file,
               amount     => DBMS_LOB.getLength (lbfq_xls_file));

            DBMS_LOB.CLOSE (lbfq_xls_file);
            l_xxd_eci_attach_tbl (1).attach_name := lc_rename_file_name;
            l_xxd_eci_attach_tbl (1).attach_mime := 'application/vnd.ms-excel';
            DBMS_LOB.createtemporary (l_xxd_eci_attach_tbl (1).attach_blob,
                                      FALSE);
            DBMS_LOB.OPEN (l_xxd_eci_attach_tbl (1).attach_blob,
                           DBMS_LOB.lob_readwrite);
            DBMS_LOB.COPY (dest_lob      => l_xxd_eci_attach_tbl (1).attach_blob,
                           src_lob       => lbbq_xls_file,
                           amount        => DBMS_LOB.getLength (lbbq_xls_file),
                           dest_offset   => 1,
                           src_offset    => 1);

            lc_smpt_host := fnd_profile.VALUE ('XXD_LSS_OUTBOUND_MAIL_SERVER');
            lc_smpt_port := 25;

            fnd_file.put_line (
               fnd_file.LOG,
               'Calling send_email_notification for Termination Report');

            xxd_okl_eci_inv_x_pk.send_email_notification (
               p_to                   => lc_to,
               p_from                 => 'noreply@delagelanden.com',
               p_subject              => 'DLL Operating Lease Pending Terminations Report',
               p_text_msg             => NULL,
               p_xxd_eci_attach_tbl   => l_xxd_eci_attach_tbl,
               p_smtp_host            => lc_smpt_host,
               p_smtp_port            => lc_smpt_port);
         END IF;

         ln_child_in_error := 0;
         ln_child_in_warn := 0;

         OPEN get_parent_sts_c (gn_conc_request_id);

         FETCH get_parent_sts_c INTO ln_child_in_error;

         CLOSE get_parent_sts_c;

         OPEN get_parent_warn_sts_c (gn_conc_request_id);

         FETCH get_parent_warn_sts_c INTO ln_child_in_warn;

         CLOSE get_parent_warn_sts_c;

         fnd_file.put_line (
            fnd_file.LOG,
               'Run Type is - '
            || p_run_type
            || ' ln_child_in_error - '
            || ln_child_in_error
            || ' ln_child_in_warn - '
            || ln_child_in_warn);

         IF p_run_type = 'FILE'
         THEN
            OPEN get_unp_cnt_c;

            FETCH get_unp_cnt_c INTO ln_reclas_cnt;

            CLOSE get_unp_cnt_c;
         ELSE
            OPEN get_unp_c;

            FETCH get_unp_c INTO ln_reclas_cnt;

            CLOSE get_unp_c;
         END IF;


         IF ln_child_in_error > 0
         THEN
            errbuf := 'Done, but with error!';
            retcode := 2;
            RETURN;
         ELSIF ln_child_in_warn > 0
         THEN
            errbuf := 'Done, but with warning(s)!';
            retcode := 1;
            RETURN;
         ELSE
            fnd_file.put_line (fnd_file.LOG, 'Child program(s) completed ');

            IF ln_reclas_cnt > 0
            THEN
               errbuf :=
                  'Program completed successfully - Some contract(s) were not be processed, refer log file or staging table for details ';
               retcode := 1;
               RETURN;
            ELSE
               errbuf := 'Program completed successfully!';
               retcode := 0;
               RETURN;
            END IF;
         END IF;

         RETURN;
      ELSIF (lc_req_data = 2)
      THEN
         ln_child_in_error := 0;
         ln_child_in_warn := 0;

         OPEN get_parent_sts_c (gn_conc_request_id);

         FETCH get_parent_sts_c INTO ln_child_in_error;

         CLOSE get_parent_sts_c;

         OPEN get_parent_warn_sts_c (gn_conc_request_id);

         FETCH get_parent_warn_sts_c INTO ln_child_in_warn;

         CLOSE get_parent_warn_sts_c;

         fnd_file.put_line (
            fnd_file.LOG,
               'Run Type is - '
            || p_run_type
            || ' ln_child_in_error - '
            || ln_child_in_error
            || ' ln_child_in_warn - '
            || ln_child_in_warn);

         IF p_run_type = 'FILE'
         THEN
            OPEN get_unp_cnt_c;

            FETCH get_unp_cnt_c INTO ln_reclas_cnt;

            CLOSE get_unp_cnt_c;
         ELSE
            OPEN get_unp_c;

            FETCH get_unp_c INTO ln_reclas_cnt;

            CLOSE get_unp_c;
         END IF;


         IF ln_child_in_error > 0
         THEN
            errbuf := 'Done, but with error!';
            retcode := 2;
            RETURN;
         ELSIF ln_child_in_warn > 0
         THEN
            errbuf := 'Done, but with warning(s)!';
            retcode := 1;
            RETURN;
         ELSE
            fnd_file.put_line (fnd_file.LOG, 'Child program(s) completed ');

            IF ln_reclas_cnt > 0
            THEN
               errbuf :=
                  'Program completed successfully - Some contract(s) were not be processed, refer log file or staging table for details ';
               retcode := 1;
               RETURN;
            ELSE
               errbuf := 'Program completed successfully!';
               retcode := 0;
               RETURN;
            END IF;
         END IF;

         RETURN;
      END IF;


      IF (gd_profile_val <> 'Y')
      THEN
         fnd_file.put_line (
            fnd_file.LOG,
            'The profile option setup does not allow the program to run for this responsibility - Please check the profile DLL_LEASE_RECLASSIFICATION_ORG for more details');
         errbuf :=
            'The profile option setup does not allow the program to run for this responsibility - Please check the profile DLL_LEASE_RECLASSIFICATION_ORG for more details';
         retcode := 2;
      ELSE
         fnd_file.put_line (
            fnd_file.LOG,
            '------------------------------------------------------Master Program---------------------------------------------------------------------------');


         -- print the parameters in the log file
         fnd_file.put_line (fnd_file.LOG,
                            'Set of Books Id : ' || p_set_of_books_id);
         fnd_file.put_line (fnd_file.LOG,
                            'Operating Unit Id : ' || p_operating_unit_id);
         fnd_file.put_line (fnd_file.LOG,
                            'Component Code: ' || p_component_code);
         fnd_file.put_line (fnd_file.LOG, 'Stage Code : ' || p_stage_code);
         fnd_file.put_line (fnd_file.LOG, 'Run Type : ' || p_run_type);
         fnd_file.put_line (fnd_file.LOG,
                            'Contract Number : ' || p_contract_number);
         fnd_file.put_line (fnd_file.LOG,
                            'Number of Proceses : ' || p_number_of_process);

         ln_batch_id :=
            gn_conc_request_id || '-' || TO_CHAR (SYSDATE, 'RRRRMMDDHHMMSS');
         fnd_file.put_line (fnd_file.LOG, 'Batch id is : ' || ln_batch_id);
         fnd_file.put_line (fnd_file.output, 'Batch id is : ' || ln_batch_id);
         fnd_file.put_line (fnd_file.LOG,
                            'New product id  is : ' || gd_pdt_name);

         --
         IF (p_run_type = 'FILE')
         THEN
            p_reprocess_flag := 'N';
            fnd_file.put_line (fnd_file.LOG, 'Truncate loader table');

            EXECUTE IMMEDIATE 'TRUNCATE TABLE xxd.xxd_ol_data_load_t';

            fnd_file.put_line (
               fnd_file.LOG,
               ' Submitting loader program to populate the temp table from file');

            ln_loader_req_id :=
               fnd_request.submit_request (
                  application   => 'XXD',
                  program       => 'XXD_GBL_SQL_LOAD',
                  description   => NULL,
                  start_time    => SYSDATE,
                  sub_request   => FALSE,
                  argument1     => NULL,                    --p_set_of_bks_id,
                  argument2     => gn_org_id,
                  argument3     => 'XXD_OL_RECLASS',
                  argument4     => 'DATA_LOAD',
                  argument5     => p_int_dir,
                  argument6     => 'XXD_ARCHIVE_TOP/WAYNE',
                  argument7     => 'INFILE',
                  argument8     => p_filename,
                  argument9     => 'XXD_TOP',
                  argument10    => 'XXD_OPL_RECLASS_RBK_C.ctl',
                  argument11    => 'Y',
                  argument12    => 'XXD_OL_DATA_LOAD_T',
                  argument15    => 'Y',
                  argument16    => 'N');
            COMMIT;

            IF (ln_loader_req_id = 0)
            THEN
               v_return_status := 1;
               fnd_file.put_line (fnd_file.LOG,
                                  'Loader request submission failed ');
               errbuf := fnd_message.get;
               retcode := v_return_status;
            ELSE
               fnd_conc_global.set_req_globals (conc_status    => 'PAUSED',
                                                request_data   => TO_CHAR (1));
               errbuf := 'Loader Request submitted! ' || errbuf;
               retcode := 0;
               fnd_file.put_line (fnd_file.output, errbuf);
               COMMIT;
            END IF;

            DECLARE
               lc_child_error_flag   VARCHAR2 (50);
               lc_child_warn_flag    VARCHAR2 (50);
               lb_child_wait         BOOLEAN;
               lc_child_phase        fnd_lookups.lookup_code%TYPE;
               -- standard output parameters
               lc_child_status       fnd_lookups.lookup_code%TYPE;
               -- standard output parameters
               lc_child_dev_phase    fnd_lookups.lookup_code%TYPE;
               -- standard output parameters
               lc_child_dev_status   fnd_lookups.lookup_code%TYPE;
               -- standard output parameters
               lc_child_message      xxd_geh_msgs_t.MESSAGE_TEXT%TYPE;

               CURSOR get_child_status_c (p_request_id NUMBER)
               IS
                  SELECT request_id
                    FROM fnd_concurrent_requests
                   WHERE parent_request_id = p_request_id;
            BEGIN
               fnd_file.put_line (fnd_file.LOG,
                                  'Waiting for child programs for Loader');

               lc_child_error_flag := NULL;
               lc_child_warn_flag := NULL;

              <<request_loop>>
               FOR lcu_child_rec IN get_child_status_c (gn_conc_request_id)
               LOOP
                 <<wait_loop>>
                  LOOP
                     lb_child_wait :=
                        fnd_concurrent.wait_for_request (
                           lcu_child_rec.request_id,
                           5,
                           15,
                           lc_child_phase,
                           lc_child_status,
                           lc_child_dev_phase,
                           lc_child_dev_status,
                           lc_child_message);

                     IF lc_child_dev_phase = 'COMPLETE'
                     THEN
                        -- check for the status of the concurrent program
                        IF    (lc_child_dev_status = 'E')
                           OR (lc_child_dev_status = 'TERMINATED')
                        THEN
                           -- set as error in child program
                           lc_child_error_flag := 'Y';
                        ELSIF lc_child_dev_status = 'WARNING'
                        THEN
                           lc_child_warn_flag := 'Y';
                        END IF;

                        EXIT wait_loop;
                     END IF;
                  END LOOP wait_loop;
               END LOOP request_loop;

               IF lc_child_error_flag = 'Y'
               THEN
                  errbuf := 'Loader child Completed in error.';
                  retcode := 2;
                  RAISE ex_load_processing;
               END IF;
            END;


            FOR lcu_exs_contracts_rec IN get_exs_contracts
            LOOP
               EXIT WHEN get_exs_contracts%NOTFOUND;

               --               ln_reclas_cnt := ln_reclas_cnt + 1;
               fnd_file.put_line (
                  fnd_file.LOG,
                     'Contract - '
                  || lcu_exs_contracts_rec.contract_number
                  || ' has already been reclassified.');

               UPDATE xxd_ol_data_load_t
                  SET comments = 'This contract has already been Reclassified',
                      batch_id = ln_batch_id
                WHERE     contract_number =
                             lcu_exs_contracts_rec.contract_number
                      AND TRUNC (batch_date) = TRUNC (SYSDATE);

               COMMIT;
            END LOOP;

            FOR lcu_evg_contracts_rec IN get_evg_contracts
            LOOP
               EXIT WHEN get_evg_contracts%NOTFOUND;

               UPDATE xxd_ol_data_load_t
                  SET comments = 'This contract is not in BOOKED status',
                      batch_id = ln_batch_id
                WHERE     contract_number =
                             lcu_evg_contracts_rec.contract_number
                      AND TRUNC (batch_date) = TRUNC (SYSDATE);

               COMMIT;
            END LOOP;

            FOR lcu_pmc_contracts_rec IN get_pmc_contracts
            LOOP
               EXIT WHEN get_pmc_contracts%NOTFOUND;

               UPDATE xxd_ol_data_load_t
                  SET comments =
                         'This contract is of a different operating unit than the current run',
                      batch_id = ln_batch_id
                WHERE     contract_number =
                             lcu_pmc_contracts_rec.contract_number
                      AND TRUNC (batch_date) = TRUNC (SYSDATE);

               COMMIT;
            END LOOP;

            FOR lcu_evg_billed_rec IN get_evg_billed_cont_c
            LOOP
               EXIT WHEN get_evg_billed_cont_c%NOTFOUND;

               UPDATE xxd_ol_data_load_t
                  SET comments =
                         'This contract has an Evergreen Rent billed stream',
                      batch_id = ln_batch_id
                WHERE     contract_number =
                             lcu_evg_billed_rec.contract_number
                      AND TRUNC (batch_date) = TRUNC (SYSDATE);

               COMMIT;
            END LOOP;

            FOR lcu_contracts_rec IN get_contracts_c (p_number_of_process)
            LOOP
               EXIT WHEN get_contracts_c%NOTFOUND;

               UPDATE xxd_ol_data_load_t
                  SET batch_id = ln_batch_id
                WHERE contract_number = lcu_contracts_rec.contract_number;

               INSERT INTO xxd_ol_rbk_temp_t (BATCH_ID,
                                              BATCH_DATE,
                                              RUN_TYPE,
                                              CONTRACT_ID,
                                              CONTRACT_NUMBER,
                                              ORIG_PDT_ID,
                                              CHILD_NUM,
                                              ENTITY,
                                              START_DATE,
                                              END_DATE,
                                              STS_CODE,
                                              CLASS,
                                              STATUS,
                                              PROCESSED_FLAG,
                                              ORG_ID,
                                              CREATION_DATE,
                                              REQUEST_ID,
                                              REPROCESS_FLAG,
                                              ERROR_MSG) --added by sherin to incorporate audit issue changes. ALl contracts from loaded for a particular batch, will be inserted with comments
                    VALUES (ln_batch_id,
                            lcu_contracts_rec.batch_date,
                            p_run_type,
                            lcu_contracts_rec.contract_id,
                            lcu_contracts_rec.contract_number,
                            lcu_contracts_rec.orig_pdt_id,
                            lcu_contracts_rec.child_number,
                            lcu_contracts_rec.entity,
                            lcu_contracts_rec.start_date,
                            lcu_contracts_rec.end_date,
                            lcu_contracts_rec.sts_code,
                            lcu_contracts_rec.class,
                            lcu_contracts_rec.status, --Pending for new contracts, NULL for reclassified/evg/pmc
                            lcu_contracts_rec.processed_flag, -- Not processed for new contracts, NULL for reclassified/evg/pmc
                            lcu_contracts_rec.org_id,
                            SYSDATE,
                            gn_conc_request_id,
                            p_reprocess_flag,
                            lcu_contracts_rec.comments);

               COMMIT;

               IF (lcu_contracts_rec.comments IS NULL) -- it should not be a reclassified/pending req/evg/pmc contract OR JUNK
               THEN
                  fnd_file.put_line (
                     fnd_file.LOG,
                     'Calling the cancel rebook/quote procedure :');

                  xxd_cancel_rbk_qte_req (lcu_contracts_rec.contract_id,
                                          gn_conc_request_id,
                                          l_canc_ret_sts,
                                          l_qte_ret_sts,
                                          lc_rbk_flag,
                                          lc_qte_flag);

                  IF (l_canc_ret_sts = 'E')
                  THEN
                     fnd_file.put_line (
                        fnd_file.LOG,
                           'Error during cancellation of rebook request for contract - '
                        || lcu_contracts_rec.contract_id);
                  END IF;

                  IF (l_qte_ret_sts = 'E')
                  THEN
                     fnd_file.put_line (
                        fnd_file.LOG,
                           'Error during cancellation of quote request for contract - '
                        || lcu_contracts_rec.contract_id);
                  END IF;

                  IF (l_qte_ret_sts <> 'E' AND l_canc_ret_sts <> 'E')
                  THEN
                     IF (lc_rbk_flag = 'Y' OR lc_qte_flag = 'Y')
                     THEN
                        fnd_file.put_line (
                           fnd_file.LOG,
                              'Not updating the product id for the contract - '
                           || lcu_contracts_rec.contract_id
                           || ' since it has a pending rebook/quote request');
                     ELSE
                        ln_temp_count := ln_temp_count + 1;
                        fnd_file.put_line (
                           fnd_file.LOG,
                           'Updating product id for the contract');

                        ln_orig_pdt_id := NULL;
                        ln_pdt_id := NULL;
                        lc_product_name := NULL;

                        OPEN get_pdt_id_c (lcu_contracts_rec.contract_id);

                        FETCH get_pdt_id_c
                        INTO ln_orig_pdt_id, ln_pdt_id, lc_product_name;

                        CLOSE get_pdt_id_c;

                        IF lc_product_name LIKE '%NON%TAX%'
                        THEN
                           UPDATE okc_rules_b
                              SET RULE_INFORMATION1 = 'LESSOR'
                            WHERE     dnz_chr_id =
                                         lcu_contracts_rec.contract_id
                                  AND rule_information_category = 'LATOWN';
                        END IF;

                        UPDATE okl_k_headers
                           SET pdt_id = ln_pdt_id, deal_type = 'LEASEOP'
                         WHERE id = lcu_contracts_rec.contract_id;

                        change_depreciation_flag_prc2 (
                           lcu_contracts_rec.contract_id,
                           lc_return_status,
                           lc_error_message);

                        COMMIT;
                     END IF;
                  END IF;
               END IF;
            END LOOP;

            fnd_file.put_line (
               fnd_file.LOG,
               'Number of contracts to be processed is = ' || ln_temp_count);

            --            OPEN get_file_cnt_c;
            --
            --            FETCH get_file_cnt_c INTO ln_temp_count;
            --
            --            CLOSE get_file_cnt_c;

            COMMIT;
         ELSIF (p_run_type = 'CONTRACT')
         THEN
            p_reprocess_flag := 'N';

            FOR lcu_contract_rec IN get_contract_c (p_contract_number)
            LOOP
               EXIT WHEN get_contract_c%NOTFOUND;
               fnd_file.put_line (fnd_file.LOG,
                                  'Inserting values into temp table');

               INSERT INTO xxd_ol_rbk_temp_t (BATCH_ID,
                                              BATCH_DATE,
                                              RUN_TYPE,
                                              CONTRACT_ID,
                                              CONTRACT_NUMBER,
                                              ORIG_PDT_ID,
                                              CHILD_NUM,
                                              ENTITY,
                                              START_DATE,
                                              END_DATE,
                                              STS_CODE,
                                              CLASS,
                                              STATUS,
                                              PROCESSED_FLAG,
                                              ORG_ID,
                                              CREATION_DATE,
                                              REQUEST_ID,
                                              REPROCESS_FLAG)
                    VALUES (ln_batch_id,
                            TRUNC (SYSDATE),
                            p_run_type,
                            lcu_contract_rec.contract_id,
                            lcu_contract_rec.contract_number,
                            lcu_contract_rec.orig_pdt_id,
                            1,
                            lcu_contract_rec.entity,
                            lcu_contract_rec.start_date,
                            lcu_contract_rec.end_date,
                            lcu_contract_rec.sts_code,
                            lcu_contract_rec.class,
                            'P',                                     --Pending
                            'N',                              -- Not processed
                            lcu_contract_rec.org_id,
                            SYSDATE,
                            gn_conc_request_id,
                            p_reprocess_flag);

               COMMIT;

               FOR lcu_get_evg_billd_rec
                  IN get_evg_billd_c (p_contract_number)
               LOOP
                  EXIT WHEN get_evg_billd_c%NOTFOUND;

                  fnd_file.put_line (
                     fnd_file.LOG,
                     'This contract has an Evergreen Rent billed stream');
                  errbuf :=
                     'Child request not submitted since this contract has an Evergreen Rent billed stream';
                  retcode := 1;

                  UPDATE xxd_ol_rbk_temp_t
                     SET error_msg =
                            'This contract has an Evergreen Rent billed stream',
                         status = 'E'
                   WHERE     contract_number = p_contract_number
                         AND run_type = 'CONTRACT'
                         AND request_id = gn_conc_request_id;

                  COMMIT;
               END LOOP;

               FOR lcu_get_exs_rec IN get_exs_contract (p_contract_number)
               LOOP
                  EXIT WHEN get_exs_contract%NOTFOUND;

                  fnd_file.put_line (
                     fnd_file.LOG,
                     'This contract has already been reclassified to Operating Lease');

                  errbuf :=
                     'Child request not submitted since this contract has already been reclassified to Operating Lease';
                  retcode := 1;

                  UPDATE xxd_ol_rbk_temp_t
                     SET error_msg =
                            'This contract has already been reclassified to Operating Lease',
                         status = 'E'
                   WHERE     contract_number = p_contract_number
                         AND run_type = 'CONTRACT'
                         AND request_id = gn_conc_request_id;

                  COMMIT;
               END LOOP;
            END LOOP;

            FOR lcu_cont_dtls_rec
               IN get_cont_dtls_c (p_contract_number, gn_conc_request_id)
            LOOP
               ln_temp_count := 1;

               fnd_file.put_line (
                  fnd_file.LOG,
                  'Calling the cancel rebook/quote procedure :');

               xxd_cancel_rbk_qte_req (lcu_cont_dtls_rec.contract_id,
                                       gn_conc_request_id,
                                       l_canc_ret_sts,
                                       l_qte_ret_sts,
                                       lc_rbk_flag,
                                       lc_qte_flag);

               IF (l_canc_ret_sts = 'E')
               THEN
                  fnd_file.put_line (
                     fnd_file.LOG,
                        'Error during cancellation of rebook request for contract - '
                     || lcu_cont_dtls_rec.contract_id);
               END IF;

               IF (l_qte_ret_sts = 'E')
               THEN
                  fnd_file.put_line (
                     fnd_file.LOG,
                        'Error during cancellation of quote request for contract - '
                     || lcu_cont_dtls_rec.contract_id);
               END IF;


               IF (l_qte_ret_sts <> 'E' AND l_canc_ret_sts <> 'E')
               THEN
                  IF (lc_rbk_flag = 'N' AND lc_qte_flag = 'N') --do not change this condition
                  THEN
                     fnd_file.put_line (
                        fnd_file.LOG,
                        'Updating product id for the contract');

                     ln_orig_pdt_id := NULL;
                     ln_pdt_id := NULL;
                     lc_product_name := NULL;

                     OPEN get_pdt_id_c (lcu_cont_dtls_rec.contract_id);

                     FETCH get_pdt_id_c
                     INTO ln_orig_pdt_id, ln_pdt_id, lc_product_name;

                     CLOSE get_pdt_id_c;

                     IF lc_product_name LIKE '%NON%TAX%'
                     THEN
                        UPDATE okc_rules_b
                           SET RULE_INFORMATION1 = 'LESSOR'
                         WHERE     dnz_chr_id = lcu_cont_dtls_rec.contract_id
                               AND rule_information_category = 'LATOWN';
                     END IF;

                     UPDATE okl_k_headers
                        SET pdt_id = ln_pdt_id, deal_type = 'LEASEOP'
                      WHERE id = lcu_cont_dtls_rec.contract_id;

                     change_depreciation_flag_prc2 (
                        lcu_cont_dtls_rec.contract_id,
                        lc_return_status,
                        lc_error_message);

                     COMMIT;
                  ELSE
                     ln_temp_count := 0; --adding so that child wont get submitted and master will complete in warning 7th september
                     fnd_file.put_line (
                        fnd_file.LOG,
                        'This contract has a pending rbk/qte request');
                     errbuf := 'This contract has a pending rbk/qte request';
                     retcode := 1;
                  END IF;
               END IF;
            END LOOP;
         ELSE
            ln_temp_count := 0;
            p_reprocess_flag := 'Y';

            FOR lcu_rep_cont_rec IN get_rep_cont_c
            LOOP
               EXIT WHEN get_rep_cont_c%NOTFOUND;
               fnd_file.put_line (
                  fnd_file.LOG,
                     'Calling the cancel rebook/quote procedure for contract  :'
                  || lcu_rep_cont_rec.contract_number);

               xxd_cancel_rbk_qte_req (lcu_rep_cont_rec.contract_id,
                                       lcu_rep_cont_rec.request_id,
                                       l_canc_ret_sts,
                                       l_qte_ret_sts,
                                       lc_rbk_flag,
                                       lc_qte_flag);

               IF (l_canc_ret_sts = 'E')
               THEN
                  fnd_file.put_line (
                     fnd_file.LOG,
                        'Error during cancellation of rebook request for contract - '
                     || lcu_rep_cont_rec.contract_id);
               END IF;

               IF (l_qte_ret_sts = 'E')
               THEN
                  fnd_file.put_line (
                     fnd_file.LOG,
                        'Error during cancellation of quote request for contract - '
                     || lcu_rep_cont_rec.contract_id);
               END IF;

               IF (l_canc_ret_sts = 'S')
               THEN
                  fnd_file.put_line (
                     fnd_file.LOG,
                        'Cancellation was successful or there are no pending rebook requests for contract - '
                     || lcu_rep_cont_rec.contract_id);

                  IF (lcu_rep_cont_rec.error_msg IN
                         ('Rebook Request Exists', 'Termination Quote Exists'))
                  THEN
                     UPDATE xxd_ol_rbk_temp_t
                        SET error_msg = NULL
                      WHERE     contract_id = lcu_rep_cont_rec.contract_id
                            AND request_id = lcu_rep_cont_rec.request_id;

                     COMMIT;
                  END IF;
               ELSIF (l_canc_ret_sts = 'P')
               THEN
                  fnd_file.put_line (
                     fnd_file.LOG,
                        'Rebook request pending for contract - '
                     || lcu_rep_cont_rec.contract_id);

                  UPDATE xxd_ol_rbk_temp_t
                     SET error_msg = 'Rebook Request Exists'
                   WHERE     contract_id = lcu_rep_cont_rec.contract_id
                         AND request_id = lcu_rep_cont_rec.request_id;

                  COMMIT;
               END IF;

               IF (l_qte_ret_sts = 'S')
               THEN
                  fnd_file.put_line (
                     fnd_file.LOG,
                        'Cancellation was successful or there are no pending quotes for contract - '
                     || lcu_rep_cont_rec.contract_id);

                  IF (lcu_rep_cont_rec.error_msg IN
                         ('Rebook Request Exists', 'Termination Quote Exists'))
                  THEN
                     UPDATE xxd_ol_rbk_temp_t
                        SET error_msg = NULL
                      WHERE     contract_id = lcu_rep_cont_rec.contract_id
                            AND request_id = lcu_rep_cont_rec.request_id;

                     COMMIT;
                  END IF;
               ELSIF (l_qte_ret_sts = 'P')
               THEN
                  fnd_file.put_line (
                     fnd_file.LOG,
                        'Quote request pending for contract - '
                     || lcu_rep_cont_rec.contract_id);

                  UPDATE xxd_ol_rbk_temp_t
                     SET error_msg = 'Termination Quote Exists'
                   WHERE     contract_id = lcu_rep_cont_rec.contract_id
                         AND request_id = lcu_rep_cont_rec.request_id;

                  COMMIT;
               END IF;
            END LOOP;

            fnd_file.put_line (
               fnd_file.LOG,
               'Proceeding to reprocess if there are eligible contracts');

            FOR lcu_repr_cont_rec IN get_repr_cont_c (p_number_of_process)
            LOOP
               ln_temp_count := ln_temp_count + 1;
               EXIT WHEN get_repr_cont_c%NOTFOUND;

               fnd_file.put_line (
                  fnd_file.LOG,
                     'Reprocessing contract - '
                  || lcu_repr_cont_rec.contract_number);

               IF lcu_repr_cont_rec.status = 'E'
               THEN
                  UPDATE xxd_ol_rbk_temp_t
                     SET reprocess_flag = p_reprocess_flag,
                         child_num = lcu_repr_cont_rec.child_num
                   WHERE     contract_number =
                                lcu_repr_cont_rec.contract_number
                         AND request_id = lcu_repr_cont_rec.request_id;

                  ln_orig_pdt_id := NULL;
                  ln_pdt_id := NULL;
                  lc_product_name := NULL;

                  OPEN get_pdt_id_c (lcu_repr_cont_rec.contract_id);

                  FETCH get_pdt_id_c
                  INTO ln_orig_pdt_id, ln_pdt_id, lc_product_name;

                  CLOSE get_pdt_id_c;

                  IF lc_product_name LIKE '%NON%TAX%'
                  THEN
                     UPDATE okc_rules_b
                        SET RULE_INFORMATION1 = 'LESSOR'
                      WHERE     dnz_chr_id = lcu_repr_cont_rec.contract_id
                            AND rule_information_category = 'LATOWN';
                  END IF;

                  UPDATE okl_k_headers
                     SET pdt_id = ln_pdt_id, deal_type = 'LEASEOP'
                   WHERE id = lcu_repr_cont_rec.contract_id;

                  change_depreciation_flag_prc2 (
                     lcu_repr_cont_rec.contract_id,
                     lc_return_status,
                     lc_error_message);
               ELSE
                  UPDATE xxd_ol_rbk_temp_t
                     SET reprocess_flag = p_reprocess_flag,
                         child_num = lcu_repr_cont_rec.child_num
                   WHERE     contract_number =
                                lcu_repr_cont_rec.contract_number
                         AND request_id = lcu_repr_cont_rec.request_id;
               END IF;

               COMMIT;
            END LOOP;

            fnd_file.put_line (
               fnd_file.LOG,
                  'Number of contracts getting reprocessed is : '
               || ln_temp_count
               || ' Please note that contracts which were submitted individually initially, and went into error, will not be reprocessed');
         END IF;

         fnd_file.put_line (
            fnd_file.LOG,
            'Number of records to be processed - ' || ln_temp_count);

         IF (ln_temp_count > 0)
         THEN
            IF (ln_temp_count >= p_number_of_process)
            THEN
               -- only if total number of records are greater than number of process we need to submit multiple child programs
               FOR i IN 1 .. p_number_of_process
               LOOP
                  fnd_file.put_line (
                     fnd_file.LOG,
                     '------------------------------------------------------Submitting Child Programs-----------------------------------------------------------------------');

                  ln_request_id :=
                     fnd_request.submit_request (
                        application   => 'XXD',
                        program       => 'XXD_OP_RECLASS_CHILD',
                        description   => NULL,
                        start_time    => NULL,
                        sub_request   => TRUE,
                        argument1     => i,
                        argument2     => p_time_out,
                        argument3     => gn_conc_request_id,
                        argument4     => p_reprocess_flag);

                  COMMIT;

                  IF (ln_request_id = 0)
                  THEN
                     v_return_status := 1;
                     -- If request submission failed, exit with error
                     fnd_file.put_line (fnd_file.LOG,
                                        'Child request submission failed ');
                     errbuf := fnd_message.get;
                     retcode := v_return_status;
                  END IF;
               END LOOP;
            ELSE
               fnd_file.put_line (
                  fnd_file.LOG,
                  '------------------------------------------------------Submitting Child Program-----------------------------------------------------------------------');

               ln_request_id :=
                  fnd_request.submit_request (
                     application   => 'XXD',
                     program       => 'XXD_OP_RECLASS_CHILD',
                     description   => NULL,
                     start_time    => NULL,
                     sub_request   => TRUE,
                     argument1     => 1,
                     argument2     => p_time_out,
                     argument3     => gn_conc_request_id,
                     argument4     => p_reprocess_flag);

               COMMIT;

               IF (ln_request_id = 0)
               THEN
                  v_return_status := 1;
                  -- If request submission failed, exit with error
                  fnd_file.put_line (fnd_file.LOG,
                                     'Child request submission failed ');
                  errbuf := fnd_message.get;
                  retcode := v_return_status;
               END IF;
            END IF;



            fnd_conc_global.set_req_globals (conc_status    => 'PAUSED',
                                             request_data   => TO_CHAR (2));
            errbuf := 'Sub-Request submitted! ' || errbuf;
            retcode := 0;
            fnd_file.put_line (fnd_file.output,
                               'Sub-Request submitted! ' || errbuf);
         ELSE
            errbuf := 'There were no records in the temp table to process';
            retcode := 2;
            fnd_file.put_line (
               fnd_file.output,
               'There were no records in the temp table. Please also note that reprocess will not pick up contracts that are unprocessed after an individual submission');
         END IF;
      END IF;

      ln_rbk_cnt := 0;
      ln_qte_cnt := 0;

      BEGIN
         fnd_file.put_line (
            fnd_file.LOG,
            'Preparing to submit the Pending Requests Report');

         OPEN get_pend_rbk_c;

         FETCH get_pend_rbk_c INTO ln_rbk_cnt;

         CLOSE get_pend_rbk_c;

         OPEN get_pend_qte_c;

         FETCH get_pend_qte_c INTO ln_qte_cnt;

         CLOSE get_pend_qte_c;

         IF (NVL (ln_rbk_cnt, 0) = 0 AND NVL (ln_qte_cnt, 0) = 0)
         THEN
            fnd_file.put_line (fnd_file.LOG, 'There are no Pending Requests');
         END IF;

         IF ln_rbk_cnt > 0
         THEN
            fnd_file.put_line (fnd_file.LOG,
                               'Preparing template for rebook request');

            lb_request_layout :=
               fnd_request.add_layout (template_appl_name   => 'XXD',
                                       template_code        => 'XXDOPLRBKREQ',
                                       template_language    => 'en',
                                       template_territory   => 'US',
                                       output_format        => 'EXCEL');

            IF NOT lb_request_layout
            THEN
               FND_FILE.PUT_LINE (
                  FND_FILE.LOG,
                  'Failure in attaching the template
                               added successfully for XXDOPLRBKREQ');
               retcode := 2;
               RETURN;
            END IF;

            ln_request_id2 :=
               fnd_request.submit_request (application   => 'XXD',
                                           program       => 'XXDOPLRBKREQ',
                                           description   => NULL,
                                           start_time    => NULL,
                                           sub_request   => TRUE);

            COMMIT;
            fnd_file.put_line (fnd_file.LOG,
                               'Rebook Request report submitted');

            IF ln_request_id2 = 0
            THEN
               fnd_file.put_line (fnd_file.LOG,
                                  'Unable to submit rebook report request');
               retcode := 1;
            END IF;
         END IF;

         IF ln_qte_cnt > 0
         THEN
            fnd_file.put_line (
               fnd_file.LOG,
               'Preparing the template for Pending Terminations report');

            lb_qte_req_layout :=
               fnd_request.add_layout (
                  template_appl_name   => 'XXD',
                  template_code        => 'XXDOPLTERMQTE',
                  template_language    => 'en',
                  template_territory   => 'US',
                  output_format        => 'EXCEL');

            IF NOT lb_qte_req_layout
            THEN
               FND_FILE.PUT_LINE (
                  FND_FILE.LOG,
                  'Failure in attaching the template for XXDOPLTERMQTE');
               retcode := 2;
               RETURN;
            END IF;

            ln_request_id3 :=
               fnd_request.submit_request (application   => 'XXD',
                                           program       => 'XXDOPLTERMQTE',
                                           description   => NULL,
                                           start_time    => NULL,
                                           sub_request   => TRUE);

            COMMIT;
            fnd_file.put_line (fnd_file.LOG,
                               'Pending Terminations report submitted');

            IF ln_request_id3 = 0
            THEN
               fnd_file.put_line (
                  fnd_file.LOG,
                  'Unable to submit child request for Termination Quotes Report');
               retcode := 1;
            END IF;
         END IF;


         IF (ln_request_id2 <> 0 OR ln_request_id3 <> 0)
         THEN
            fnd_conc_global.set_req_globals (conc_status    => 'PAUSED',
                                             request_data   => TO_CHAR (4));
         END IF;
      END;

      RETURN;
   EXCEPTION
      WHEN ex_load_processing
      THEN
         fnd_file.put_line (fnd_file.LOG,
                            'Error in Loader program ' || SQLERRM);
         errbuf := 'Error in loader program';
         retcode := 2;
         RETURN;
      WHEN OTHERS
      THEN
         ROLLBACK;
         fnd_file.put_line (
            fnd_file.LOG,
               'Master program went into exception, check logs for more details'
            || SQLERRM);
         fnd_file.put_line (
            fnd_file.LOG,
               'Inside exception '
            || DBMS_UTILITY.FORMAT_ERROR_BACKTRACE ()
            || SQLERRM);
         errbuf := 'Error in Master program';
         retcode := 2;
         RETURN;
   END ol_rbk_master_proc;

   /*=============================================================================+
          | PROCEDURE:OL_RBK_CHILD_PROC
          |
          | DESCRIPTION: This PL/SQL procedure is the child procedure which submits each contract for the rebook process
          |
          | PARAMETERS:
          | IN:      p_number_of_process
          |OUT:
          |       retcode  : Returns status of the procedure
          |       errbuf : Return the error message
          |
          | SCOPE:     PUBLIC procedure
          |
          | EXTERNAL PROCEDURES/FUNCTIONS USED:
          |   OWNER              NAME                                TYPE
          |   ---------------    ------------------------------      -----------
          |   NA
          |
          | HISTORY:
          |  WHO               WHEN                    WHAT
          |  --------------    ---------------         -------------------------------
          |  Sherin           06-May-2016            Initial Version
          |
          +============================================================================*/

   PROCEDURE ol_rbk_child_proc (
      errbuf                   OUT VARCHAR2,
      retcode                  OUT VARCHAR2,
      p_number_of_process   IN     NUMBER,
      p_time_out            IN     NUMBER DEFAULT 40,
      p_conc_req_id         IN     NUMBER,
      p_reprocess_flag      IN     VARCHAR2 DEFAULT 'N')
   IS
      CURSOR get_child_process_c (
         p_number_of_process    NUMBER,
         p_conc_req_id          NUMBER)
      IS
         SELECT contract_id, child_num, status
           FROM xxd_ol_rbk_temp_t
          WHERE     child_num = p_number_of_process
                AND processed_flag = 'N'
                AND status <> 'E'
                AND org_id = gn_org_id
                AND NVL (
                       Fnd_Profile.VALUE ('DLL_LEASE_RECLASSIFICATION_ORG'),
                       'N') = 'Y'
                AND request_id = p_conc_req_id;

      -- For reprocessing

      CURSOR get_rep_cont_c (
         p_number_of_process NUMBER)
      IS
         SELECT tmp.contract_number,
                tmp.contract_id,
                tmp.request_id,
                tmp.status,
                tmp.child_num
           FROM xxd_ol_rbk_temp_t tmp
          WHERE     tmp.processed_flag = 'N'
          --Start of modification by Priya for Reprocess
          AND child_num = p_number_of_process
          AND NVL (Fnd_Profile.VALUE ('DLL_LEASE_RECLASSIFICATION_ORG'),
                       'N') = 'Y'
          AND error_msg NOT IN
                       ('Contract not available to process',
                        'This contract has an Evergreen Rent billed stream')             
          --End of modification by Priya for Reprocess
                AND tmp.run_type = 'FILE'
                AND error_msg NOT IN
                       ('Rebook Request Exists', 'Termination Quote Exists') --exclude these
                AND request_id =
                       (SELECT MAX (request_id)    --latest unprocessed record
                          FROM xxd_ol_rbk_temp_t t
                         WHERE     t.contract_number = tmp.contract_number
                               AND processed_flag = 'N')
                AND NOT EXISTS
                           (SELECT 1
                              FROM xxd_ol_rbk_temp_t temp
                             WHERE     temp.contract_number =
                                          tmp.contract_number
                                   AND processed_flag = 'Y');

      CURSOR get_temp_dtls_c (p_contract_id NUMBER, p_conc_req_id NUMBER)
      IS
         SELECT *
           FROM xxd_ol_rbk_temp_t
          WHERE contract_id = p_contract_id AND request_id = p_conc_req_id;

      CURSOR get_rbk_dtls_c (
         p_contract_id NUMBER)
      IS
         (SELECT DISTINCT
                 FIRST_VALUE (khr_id_new) OVER (ORDER BY khr_id_new DESC)
                    rbk_chr_id
            FROM okl_trx_contracts
           WHERE     khr_id_old = p_contract_id
                 AND rbr_code = 'RECLASS_OPL'
                 AND tsu_code = 'PROCESSED');

      ln_rbk_contract_id   okc_k_headers_b.id%TYPE;
      lc_return_status     VARCHAR2 (10);
      lc_error_message     VARCHAR2 (3000);
      ln_conc_request_id   fnd_concurrent_requests.request_id%TYPE;
      lcu_temp_dtls_rec    get_temp_dtls_c%ROWTYPE;
      lc_msg_data          VARCHAR2 (1000);
      ln_rbk_failed        NUMBER;
      ln_fa_failed         NUMBER;
      ln_rec_cnt           NUMBER;
   BEGIN
      ln_conc_request_id := fnd_global.conc_request_id;
      ln_rbk_failed := 0;
      ln_fa_failed := 0;
      ln_rec_cnt := 0;

      IF p_reprocess_flag = 'N'
      THEN
         FOR lcu_data_rec
            IN get_child_process_c (p_number_of_process, p_conc_req_id)
         LOOP
            EXIT WHEN get_child_process_c%NOTFOUND;

            --            ln_rec_cnt := ln_rec_cnt + 1;

            IF (lcu_data_rec.status = 'P')
            THEN
               fnd_file.put_line (
                  fnd_file.LOG,
                     'Calling the rebook procedure for contract : '
                  || lcu_data_rec.contract_id);

               xxd_ol_rbk_proc (lcu_data_rec.contract_id,
                                lcu_data_rec.child_num,
                                p_time_out,
                                p_conc_req_id,
                                lc_error_message,
                                lc_return_status);

               IF (lc_return_status = 'E')
               THEN
                  fnd_file.put_line (
                     fnd_file.output,
                        'Contract ID - '
                     || lcu_data_rec.contract_id
                     || ' failed in rebook or asset update- check the xxd_ol_rbk_temp_t for more details');
                  ln_rbk_failed := ln_rbk_failed + 1;
               ELSIF (lc_return_status = 'W')
               THEN
                  fnd_file.put_line (
                     fnd_file.output,
                        'Contract ID - '
                     || lcu_data_rec.contract_id
                     || ' errored for one of the data updates - check the xxd_ol_rbk_temp_t for more details');
               END IF;
            END IF;

            COMMIT;
         END LOOP;
      ELSE
         FOR lcu_rep_rec IN get_rep_cont_c (p_number_of_process)
         LOOP
            EXIT WHEN get_rep_cont_c%NOTFOUND;

            --            ln_rec_cnt := ln_rec_cnt + 1;

            IF (lcu_rep_rec.status IN ('P', 'E'))
            THEN
               fnd_file.put_line (
                  fnd_file.LOG,
                     'Calling the rebook procedure for contract : '
                  || lcu_rep_rec.contract_id);

               xxd_ol_rbk_proc (lcu_rep_rec.contract_id,
                                lcu_rep_rec.child_num,
                                p_time_out,
                                lcu_rep_rec.request_id,
                                lc_error_message,
                                lc_return_status);

               IF (lc_return_status = 'E')
               THEN
                  fnd_file.put_line (
                     fnd_file.output,
                        'Contract ID - '
                     || lcu_rep_rec.contract_id
                     || ' failed in rebook or asset value update - check the xxd_ol_rbk_temp_t for more details');
                  ln_rbk_failed := ln_rbk_failed + 1;
               ELSIF (lc_return_status = 'W')
               THEN
                  fnd_file.put_line (
                     fnd_file.output,
                        'Contract ID - '
                     || lcu_rep_rec.contract_id
                     || ' errored for one of the data updates - check the xxd_ol_rbk_temp_t for more details');
               ELSE
                  fnd_file.put_line (fnd_file.LOG,
                                     'Reprocess successfully completed');

                  OPEN get_temp_dtls_c (lcu_rep_rec.contract_id,
                                        lcu_rep_rec.request_id); --changed with reprocess

                  FETCH get_temp_dtls_c INTO lcu_temp_dtls_rec;

                  CLOSE get_temp_dtls_c;

                  lcu_temp_dtls_rec.status := 'S';
                  lcu_temp_dtls_rec.processed_flag := 'Y';
                  lcu_temp_dtls_rec.error_msg := 'Reprocessed with no errrors';
                  xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);
               END IF;
            ELSIF (lcu_rep_rec.status = 'R')
            THEN
               ln_rbk_contract_id := NULL;

               OPEN get_rbk_dtls_c (lcu_rep_rec.contract_id);

               FETCH get_rbk_dtls_c INTO ln_rbk_contract_id;

               CLOSE get_rbk_dtls_c;

               fnd_file.put_line (
                  fnd_file.LOG,
                     'The contract has already been reclassified to Operating Lease with contract_id --> '
                  || ln_rbk_contract_id
                  || '. Calling procedure to update FA books');
               change_depreciation_flag_prc (lcu_rep_rec.contract_id,
                                             ln_rbk_contract_id,
                                             lc_return_status,
                                             lc_msg_data);

               OPEN get_temp_dtls_c (lcu_rep_rec.contract_id,
                                     lcu_rep_rec.request_id); --changed with reprocess

               FETCH get_temp_dtls_c INTO lcu_temp_dtls_rec;

               CLOSE get_temp_dtls_c;

               IF (lc_return_status = 'S')
               THEN
                  fnd_file.put_line (
                     fnd_file.LOG,
                     'FA update succesfully completed - calling the cta adjustment procedure');
                  create_fa_adj_cta_asst_prc (lcu_rep_rec.contract_id,
                                              lc_return_status,
                                              lc_msg_data);

                  --if successful, proceeding with cta adjustment
                  IF (lc_return_status = 'S')
                  THEN
                     fnd_file.put_line (
                        fnd_file.LOG,
                        'FA adjustment for CTA assets succesfully completed');

                     lcu_temp_dtls_rec.status := 'S';
                     lcu_temp_dtls_rec.processed_flag := 'Y';
                     lcu_temp_dtls_rec.error_msg :=
                        'Reprocessed with no errors';
                     xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);
                  ELSE
                     ln_fa_failed := ln_fa_failed + 1;
                     fnd_file.put_line (
                        fnd_file.LOG,
                        'FA adjustment for CTA assets not successful'); -- no update to the status of the contract, it will still be R-N
                     lcu_temp_dtls_rec.error_msg :=
                           lcu_temp_dtls_rec.error_msg
                        || 'FA adjustment for CTA assets not successful';
                     xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);
                  END IF;
               --if FA update is not succesful, we are not doing CTA either
               ELSE
                  fnd_file.put_line (
                     fnd_file.output,
                        'Contract ID - '
                     || lcu_rep_rec.contract_id
                     || ' failed in FA update');
                  ln_fa_failed := ln_fa_failed + 1;
               END IF;
            END IF;

            COMMIT;
         END LOOP;
      END IF;

      --      IF ln_rec_cnt > 0
      --      THEN
      IF (ln_rbk_failed > 0)
      THEN
         errbuf :=
               'Rebook procedure completed in error for '
            || ln_rbk_failed
            || ' contracts. See ouput file for more details';
         retcode := 2;
      ELSIF (ln_fa_failed > 0)
      THEN
         errbuf :=
               'FA update completed in error for '
            || ln_fa_failed
            || ' contracts. See ouput file for more details';
         retcode := 1;
      ELSE
         errbuf := 'Program completed, check Master Program Log for details.';
         retcode := 0;
      END IF;
   --      ELSE
   --         retcode := 1;
   --         errbuf := 'No records to process in child';
   --         fnd_file.put_line (fnd_file.LOG, 'No records to process');
   --      END IF;
   --      CLOSE get_rep_cont_c;
   --      CLOSE get_child_process_c;

   EXCEPTION
      WHEN OTHERS
      THEN
         fnd_file.put_line (fnd_file.LOG, 'Error in child procedure');

         errbuf :=
               'Exception in child procedure - '
            || SUBSTR (lc_error_message, 1, 1000);
         retcode := 2;
         ROLLBACK;
   END ol_rbk_child_proc;

   /*=============================================================================+
          | PROCEDURE: XXD_OL_RBK_PROC
          |
          | DESCRIPTION: This PL/SQL procedure is the procedure which performs the rebook
          |
          | PARAMETERS:
          | IN:      p_contract_id
          | IN:      p_child_num
          |OUT:
          |       x_error_message  : Returns status of the procedure
          |       x_return_status : Return the error message
          |
          | SCOPE:     PUBLIC procedure
          |
          | EXTERNAL PROCEDURES/FUNCTIONS USED:
          |   OWNER              NAME                                TYPE
          |   ---------------    ------------------------------      -----------
          |   NA
          |
          | HISTORY:
          |  WHO               WHEN                    WHAT
          |  --------------    ---------------         -------------------------------
          |  Sherin           06-May-2016            Initial Version
          |
          +============================================================================*/

   PROCEDURE xxd_ol_rbk_proc (p_contract_id     IN     NUMBER,
                              p_child_num       IN     NUMBER,
                              p_time_out        IN     NUMBER DEFAULT 40,
                              p_conc_req_id     IN     NUMBER,
                              x_error_message      OUT VARCHAR2,
                              x_return_status      OUT VARCHAR2)
   AS
      CURSOR get_qa_id_c (p_contract_id NUMBER)
      IS
         SELECT qcl_id qcl_id, sts_code status_code
           FROM okc_k_headers_b
          WHERE ID = p_contract_id;

      CURSOR get_rbk_date_c (p_contract_id NUMBER)
      IS
         SELECT start_date
           FROM okc_k_headers_b
          WHERE ID = p_contract_id;

      CURSOR get_user_c (p_user_id NUMBER)
      IS
         SELECT user_name
           FROM fnd_user
          WHERE user_id = p_user_id;

      CURSOR get_temp_dtls_c (p_contract_id NUMBER, p_conc_Req_id NUMBER)
      IS
         SELECT *
           FROM xxd_ol_rbk_temp_t
          WHERE contract_id = p_contract_id AND request_id = p_conc_req_id;


      lc_return_status              VARCHAR2 (1);
      lc_ret_sts                    VARCHAR2 (1);
      ln_msg_count                  NUMBER;
      l_msg_data                    VARCHAR2 (2000);
      gc_error_message              xxd_geh_msgs_t.MESSAGE_TEXT%TYPE;
      lc_err_msg                    xxd_geh_msgs_t.MESSAGE_TEXT%TYPE;
      lx_tcnv_rec                   Okl_Contract_Rebook_Pub.tcnv_rec_type;
      lc_msg_tbl                    Okl_Qa_Check_Pub.msg_tbl_type;
      ln_rebook_chr_id              NUMBER;
      ln_contract_id                NUMBER := 0;
      lc_contract_status            VARCHAR2 (20);
      lc_contract_status1           VARCHAR2 (20);
      lcu_qa_id_rec                 get_qa_id_c%ROWTYPE;
      lc_process_status             VARCHAR2 (20);
      ln_time_out                   NUMBER;
      lc_err_mess                   VARCHAR2 (200);
      gc_return_status              VARCHAR2 (1);
      gn_rebook_chr_id              NUMBER;
      gn_msg_count                  NUMBER;
      ln_interval                   NUMBER;
      ln_loop_cnt                   NUMBER;
      ln_success_count              NUMBER := 0;
      ln_total_count                NUMBER := 0;
      ln_timeout                    NUMBER;
      ln_trx_number                 okl_stream_interfaces.transaction_number%TYPE;
      lc_trx_status                 VARCHAR2 (30);
      lc_rev_status                 VARCHAR2 (10);
      ex_load_processing            EXCEPTION;
      ex_exit_processing            EXCEPTION;
      ex_abd_processing             EXCEPTION;
      ex_req_processing             EXCEPTION;
      ex_rbk_processing             EXCEPTION;
      ex_qa_processing              EXCEPTION;
      ex_strm_processing            EXCEPTION;
      ex_appr_processing            EXCEPTION;
      ex_act_processing             EXCEPTION;
      ex_upd_processing             EXCEPTION;
      ex_non_processing             EXCEPTION;
      ex_non_lines_processing       EXCEPTION;
      ln_org_id                     NUMBER;
      lt_rev_tbl                    okl_transaction_pub.rev_tbl_type;
      ld_date                       DATE := NULL;
      lc_msg_data                   VARCHAR2 (2000);
      lc_orig_product_name          okl_products.name%TYPE;
      ln_orig_pdt_id                okl_products.id%TYPE;
      lcu_temp_dtls_rec             get_temp_dtls_c%ROWTYPE;


      CURSOR get_orig_details (
         p_contract_id    NUMBER,
         p_request_id     NUMBER)
      IS
         SELECT op.name product_name, op.id pdt_id
           FROM xxd_ol_rbk_temp_t xort, okl_products op
          WHERE     xort.orig_pdt_id = op.id
                AND xort.contract_id = p_contract_id
                AND request_id = p_request_id;

      CURSOR get_ver_num (p_contract_id NUMBER)
      IS
         SELECT vers.major_version
           FROM okc_k_vers_numbers_v vers
          WHERE vers.chr_id = p_contract_id;

      CURSOR get_rebook_req_id_c (
         p_contract_id NUMBER)
      IS
         SELECT otr.ID,
                otr.object1_id1,
                otr.object1_id2,
                otr.jtot_object1_code,
                otr.dnz_khr_id,
                otr.request_type_code,
                otr.apply_to_code,
                otr.start_date,
                otr.end_date,
                otr.term_duration,
                otr.amount,
                otr.currency_code,
                otr.subsidy_yn,
                otr.cash_applied_yn,
                otr.object_version_number,
                otr.attribute_category,
                otr.attribute1,
                otr.attribute2,
                otr.attribute3,
                otr.attribute4,
                otr.attribute5,
                otr.attribute6,
                otr.attribute7,
                otr.attribute8,
                otr.attribute9,
                otr.attribute10,
                otr.attribute11,
                otr.attribute12,
                otr.attribute13,
                otr.attribute14,
                otr.attribute15,
                otr.org_id,
                otr.request_id,
                otr.program_application_id,
                otr.program_id,
                otr.program_update_date,
                otr.created_by,
                otr.creation_date,
                otr.last_updated_by,
                otr.last_update_date,
                otr.last_update_login,
                otr.minimum_rate,
                otr.maximum_rate,
                otr.tolerance,
                otr.adjustment_frequency_code,
                otr.base_rate,
                otr.index_name,
                otr.variable_method_code,
                otr.adder,
                otr.days_in_year,
                otr.days_in_month,
                otr.interest_method_code,
                otr.interest_start_date,
                otr.method_of_calculation_code,
                otr.request_number,
                otr.date_of_conversion,
                otr.variable_rate_yn,
                otr.request_status_code,
                otr.yield,
                otr.residual,
                otr.comments,
                otr.payment_frequency_code,
                otr.restructure_date,
                otr.past_due_yn,
                otr.request_reason_code,
                otr.parent_khr_id,
                otr.yield_type,
                otr.payment_amount,
                otr.payment_date,
                otr.paydown_type,
                otr.currency_conversion_rate,
                otr.currency_conversion_type,
                otr.currency_conversion_date,
                otr.lsm_id,
                otr.receipt_id,
                otr.tcn_id
           FROM okl_trx_requests otr
          WHERE     otr.dnz_khr_id = p_contract_id
                AND otr.request_type_code = 'DLL_PROPOSED_RB_REQ'
                AND otr.request_status_code = 'APPROVED'
                AND EXISTS
                       (SELECT 1
                          FROM okl_trx_contracts otc,
                               xxd_okl_trx_contracts_t xotc
                         WHERE     khr_id = otr.dnz_khr_id
                               AND xotc.tcn_id = otc.ID
                               AND xotc.request_number = otr.request_number
                               AND xotc.request_number IS NOT NULL);

      --Notes

      ln_jtf_note_id                jtf_notes_b.jtf_note_id%TYPE;
      ln_user_id                    NUMBER := fnd_global.user_id;
      ln_user_name                  fnd_user.user_name%TYPE;
      ln_notes_created              VARCHAR2 (400)
         := ' with rebook reason Reclassify to Operating Lease ';
      lc_jtf_ret_sts                VARCHAR2 (1);
      ln_jtf_msg_cnt                NUMBER := 0;
      lc_jtf_msg_data               VARCHAR2 (2000);
      l_ver_num                     okc_k_vers_numbers_v.major_version%TYPE;
      p_trqv_rec                    okl_trx_requests_pub.trqv_rec_type;
      x_trqv_rec                    okl_trx_requests_pub.trqv_rec_type; --okl_trx_requests%ROWTYPE;
      l_trqv_rec                    okl_trx_requests_pub.trqv_rec_type;
      lx_trqv_rec                   okl_trx_requests_pub.trqv_rec_type;
      p_xxd_okl_trx_contracts_rec   xxd_okl_trx_contracts_t_pk.xxd_okl_trx_contracts_rec;
      x_xxd_okl_trx_contracts_rec   xxd_okl_trx_contracts_t_pk.xxd_okl_trx_contracts_rec;
      lp_x_request_details_rec      xxd_okl_trx_requests_t_pk.xxd_okl_trx_requests_rec;
      lx_x_request_details_rec      xxd_okl_trx_requests_t_pk.xxd_okl_trx_requests_rec;
      x_request_number              okl_trx_requests.request_number%TYPE;
      x_request_id                  okl_trx_requests.id%TYPE;

      p_accum_deprn                 NUMBER;
      lc_dummy_stat                 VARCHAR (10) := NULL;
      lc_dummy_err_msg              VARCHAR2 (3000) := NULL;

      --Moving the accrual cursors to after reclassification

      ln_rental_accrual             NUMBER := 0;
      ln_rental_non_accrual         NUMBER := 0;
      ln_tot_accrual                NUMBER := 0;

      CURSOR get_rental_accrual_c (
         p_contract_id NUMBER)
      IS
         SELECT NVL (SUM (ose.amount), 0)
           FROM okl_strm_type_b sty, okl_streams stm, okl_strm_elements ose
          WHERE     1 = 1
                AND sty.id = stm.sty_id
                AND sty.code = 'RENTAL ACCRUAL'
                AND stm.id = ose.stm_id
                AND TO_CHAR (ose.stream_element_date, 'MM-RRRR') IN
                       (SELECT TO_CHAR (oc.date_accrual, 'MM-RRRR')
                          FROM okl_trx_contracts oc,
                               okl_txl_cntrct_lns ocl,
                               fnd_lookup_values_vl fnd
                         WHERE     1 = 1
                               AND oc.khr_id = stm.khr_id
                               AND oc.tcn_type = 'ACL'
                               AND oc.accrual_activity IN
                                      ('ACCRUAL', 'CATCH-UP')
                               AND oc.ID = ocl.tcn_id
                               AND fnd.attribute1 = ocl.sty_id
                               AND fnd.lookup_code = 'ACCRUED_LEASE_INCOME'
                               AND fnd.lookup_type =
                                      'DLL_CUSTOM_FORMULAE_STREAMS'
                               AND fnd.attribute_category =
                                      'DLL_CUSTOM_FORMULAE_STREAMS'
                               AND ocl.khr_id = oc.khr_id)
                AND stm.khr_id = p_contract_id
                AND stm.say_code = 'CURR'
                AND stm.active_yn = 'Y';

      CURSOR get_rental_non_acc_c (
         p_contract_id NUMBER)
      IS
         SELECT NVL (SUM (ose.amount), 0)
           FROM okl_strm_type_b sty, okl_streams stm, okl_strm_elements ose
          WHERE     1 = 1
                AND sty.id = stm.sty_id
                AND sty.code = 'RENTAL ACCRUAL'
                AND stm.id = ose.stm_id
                AND TO_CHAR (ose.stream_element_date, 'MM-RRRR') IN
                       (SELECT TO_CHAR (oc.date_accrual, 'MM-RRRR')
                          FROM okl_trx_contracts oc,
                               okl_txl_cntrct_lns ocl,
                               fnd_lookup_values_vl fnd
                         WHERE     1 = 1
                               AND oc.khr_id = stm.khr_id
				--Start of modification by Priya on 09-Jan-2017
                                --AND oc.tcn_type = 'ACL'
                                AND oc.tcn_type = 'OPLR'
				--End of modification by Priya on 09-Jan-2017
                               AND oc.accrual_activity = 'NON-ACCRUAL'
                               AND oc.ID = ocl.tcn_id
                               AND fnd.attribute1 = ocl.sty_id
                               AND fnd.lookup_code = 'ACCRUED_LEASE_INCOME'
                               AND fnd.lookup_type =
                                      'DLL_CUSTOM_FORMULAE_STREAMS'
                               AND fnd.attribute_category =
                                      'DLL_CUSTOM_FORMULAE_STREAMS'
                               AND ocl.khr_id = oc.khr_id)
                AND stm.khr_id = p_contract_id
                AND stm.say_code = 'CURR'
                AND stm.active_yn = 'Y';

      CURSOR get_total_accrual_c (
         p_contract_id NUMBER)
      IS
         SELECT SUM (amount)
           FROM okl_streams os, okl_strm_elements ose, okl_strm_type_b ost
          WHERE     ose.stm_id = os.ID
                AND os.khr_id = p_contract_id
                AND os.say_code = 'CURR'
                AND os.active_yn = 'Y'
                AND os.sty_id = ost.ID
                AND ost.code = 'RENTAL ACCRUAL';

      --Start Modification for Non-Accrual changes

      CURSOR get_non_accrual_c (
         p_contract_id NUMBER)
      IS
         SELECT a.*
           FROM okl_trx_contracts a
          WHERE     1 = 1
                AND a.khr_id = p_contract_id
                AND a.tcn_type = 'ACL'
                AND a.accrual_activity = 'NON-ACCRUAL';

      CURSOR get_non_accrual_lines_c (
         p_contract_id    NUMBER,
         p_tcn_id         okl_trx_contracts.id%TYPE)
      IS
         SELECT a.*
           FROM okl_txl_cntrct_lns a, okl_trx_contracts b
          WHERE     1 = 1
                AND b.khr_id = p_contract_id
                AND b.khr_id = a.khr_id
                AND b.id = a.tcn_id
                AND a.tcn_id = p_tcn_id;

      CURSOR get_lin_lkp_c
      IS
         SELECT lookup_code
           FROM fnd_lookup_values
          WHERE     lookup_type = 'OKL_TCL_TYPE'
                AND language = 'US'
                AND meaning = 'Operating Lease Reclassification';

      CURSOR get_hdr_lkp_c
      IS
         SELECT lookup_code
           FROM fnd_lookup_values
          WHERE     lookup_type = 'OKL_TCN_TYPE'
                AND language = 'US'
                AND meaning = 'Operating Lease Reclassification';



      SUBTYPE tcnv_rec_type IS okl_tcn_pvt.tcnv_rec_type;

      SUBTYPE tclv_rec_type IS okl_tcl_pvt.tclv_rec_type;

      l_rule_result                 VARCHAR2 (20);
      lc_non_acc_status             VARCHAR2 (30);
      lcn_return_status             VARCHAR2 (1);
      ln_lin_lkp_code               VARCHAR2 (10);
      ln_hdr_lkp_code               VARCHAR2 (10);
      lc_non_accrual_rec            get_non_accrual_c%ROWTYPE;
      lc_non_accrual_lines_rec      get_non_accrual_lines_c%ROWTYPE;
      l_tcnv_rec                    tcnv_rec_type;
      lcx_tcnv_rec                  tcnv_rec_type;
      lcn_ret_sts                   VARCHAR2 (1) := FND_API.G_RET_STS_SUCCESS;
      lcv_ret_sts                   VARCHAR2 (1) := FND_API.G_RET_STS_SUCCESS;
      l_tclv_rec                    tclv_rec_type;
      lcx_tclv_rec                  tclv_rec_type;
      --End Modification for Non-Accrual changes
      --Start of modification by Priya on 05-Jan-2017 for OPLR
      l_tabv_tbl_in                 okl_trns_acc_dstrs_pub.tabv_tbl_type;
      l_tabv_tbl_out                okl_trns_acc_dstrs_pub.tabv_tbl_type;
      l_api_version                 NUMBER := 1.0;
      l_id                          NUMBER := 0;
      i                             NUMBER := 0;

      CURSOR dist_csr (
         p_contract_id OKC_K_HEADERS_B.ID%TYPE)
      IS
         SELECT otac.ID
           FROM okl_trns_acc_dstrs otac,
                okl_txl_cntrct_lns otcl,
                okl_trx_contracts otc,
                okl_strm_type_b ost
          WHERE     1 = 1
                AND otc.id = otcl.tcn_id
                AND otcl.id = otac.source_id
                AND otc.khr_id = p_contract_id
                AND otc.tcn_type = 'OPLR'
                AND ost.id = otcl.sty_id;

      dist_rec                      dist_csr%ROWTYPE;
   --End of modification by Priya on 05-Jan-2017 for OPLR

   BEGIN
      fnd_file.put_line (fnd_file.LOG,
                         'Contract Rebook for ' || p_contract_id);

      --      fnd_profile.put('OKL_K_ASSET_CORP_BOOK','DLL US LS CORP');

      ln_contract_id := p_contract_id;
      lc_process_status := 'FAILED';
      lc_contract_status := 'PROGRESS';

      SELECT authoring_org_id
        INTO ln_org_id
        FROM okc_k_headers_b
       WHERE id = ln_contract_id;

      OPEN get_rbk_date_c (p_contract_id);

      FETCH get_rbk_date_c INTO ld_date;

      CLOSE get_rbk_date_c;


      OPEN get_user_c (ln_user_id);

      FETCH get_user_c INTO ln_user_name;

      CLOSE get_user_c;

      -- Creating a rebook request in the system for the contract

      fnd_file.put_line (
         fnd_file.LOG,
         'Creating a Rebook Request in the system for the contract');
      DBMS_APPLICATION_INFO.set_client_info (ln_org_id);
      p_trqv_rec.dnz_khr_id := p_contract_id;
      p_trqv_rec.request_reason_code := 'RECLASS_OPL';
      p_trqv_rec.request_type_code := 'DLL_PROPOSED_RB_REQ';
      p_trqv_rec.object_version_number := 1;
      p_trqv_rec.org_id := fnd_profile.VALUE ('ORG_ID');
      p_trqv_rec.created_by := ln_user_id;
      p_trqv_rec.creation_date := SYSDATE;
      p_trqv_rec.last_updated_by := ln_user_id;
      p_trqv_rec.last_update_date := SYSDATE;
      p_trqv_rec.last_update_login := NULL;
      p_trqv_rec.request_status_code := 'APPROVED';
      p_trqv_rec.comments :=
            'Contract Number :'
         || ' '
         || p_contract_id
         || CHR (10)
         || 'Reason Code :'
         || ' '
         || 'RECLASS_OPL'
         || CHR (10)
         || 'Operating Lease Reclassification'
         || CHR (10)
         || 'User :'
         || ' '
         || ln_user_name
         || CHR (10)
         || 'Date :'
         || ' '
         || TO_CHAR (SYSDATE, 'DD-Mon-YYYY HH24:MI:SS');
      okl_trx_requests_pub.insert_trx_requests (
         p_api_version     => 1.0,
         p_init_msg_list   => okl_api.g_false,
         x_return_status   => x_return_status,
         x_msg_count       => ln_msg_count,
         x_msg_data        => l_msg_data,
         p_trqv_rec        => p_trqv_rec,
         x_trqv_rec        => x_trqv_rec);

      IF lc_return_status <> Fnd_Api.g_ret_sts_success
      THEN
         fnd_file.put_line (fnd_file.LOG,
                            'Rebook Request creation is not successful');
         gc_error_message := 'Rebook Request creation is not successful';

         IF ln_msg_count >= 1
         THEN
           <<err_loop>>
            FOR i IN 1 .. ln_msg_count
            LOOP
               gc_error_message :=
                  SUBSTR (
                        gc_error_message
                     || l_msg_data
                     || ':'
                     || Fnd_Msg_Pub.get (i, 'F'),
                     1,
                     1950);
            END LOOP err_loop;
         END IF;

         RAISE ex_req_processing;
      ELSE
         fnd_file.put_line (
            fnd_file.LOG,
               'The Rebook Request created successfully for contract '
            || ln_contract_id);

         x_return_status := 'S';
         x_error_message := NULL;
      END IF;

      x_request_number := x_trqv_rec.request_number;
      x_request_id := x_trqv_rec.id;

      fnd_file.put_line (
         fnd_file.LOG,
            'The rebook request created for the contract is - '
         || x_request_number
         || ' and id is '
         || x_request_id);

      fnd_file.put_line (fnd_file.LOG,
                         'Creating Rebook Contract for ' || p_contract_id);

      fnd_file.put_line (
         fnd_file.LOG,
         'current status of the rebook is - ' || lc_contract_status);

      DBMS_APPLICATION_INFO.set_client_info (ln_org_id);
      Okl_Contract_Rebook_Pub.create_txn_contract (
         p_api_version          => 1.0,
         p_init_msg_list        => Okc_Api.g_true,
         x_return_status        => lc_return_status,
         x_msg_count            => ln_msg_count,
         x_msg_data             => gc_error_message,
         p_from_chr_id          => ln_contract_id,
         p_rebook_reason_code   => 'RECLASS_OPL',
         p_rebook_description   => '',
         p_trx_date             => ld_date,
         x_tcnv_rec             => lx_tcnv_rec,
         x_rebook_chr_id        => ln_rebook_chr_id);

      IF lc_return_status <> Fnd_Api.g_ret_sts_success
      THEN
         fnd_file.put_line (fnd_file.LOG,
                            'Rebook copy creation is not successful');
         gc_error_message := 'Rebook copy creation is not successful';

         IF ln_msg_count >= 1
         THEN
           <<err_loop>>
            FOR i IN 1 .. ln_msg_count
            LOOP
               gc_error_message :=
                  SUBSTR (
                     gc_error_message || ':' || Fnd_Msg_Pub.get (i, 'F'),
                     1,
                     1950);
            END LOOP err_loop;
         END IF;

         RAISE ex_rbk_processing;
      ELSE
         fnd_file.put_line (
            fnd_file.LOG,
               'The Rebook copy created for contract '
            || ln_contract_id
            || ' is  - '
            || ln_rebook_chr_id);

         x_return_status := 'S';
         x_error_message := NULL;
      END IF;

      fnd_file.put_line (
         fnd_file.LOG,
         'Creating the link between okl_trx_requests and okl_trx_contracts by inserting record in xxd_okl_trx_contracts_t');

      OPEN get_ver_num (p_contract_id);

      FETCH get_ver_num INTO l_ver_num;

      CLOSE get_ver_num;

      p_xxd_okl_trx_contracts_rec.id := NULL;
      p_xxd_okl_trx_contracts_rec.tcn_id := lx_tcnv_rec.id;
      p_xxd_okl_trx_contracts_rec.request_number := x_request_number;
      p_xxd_okl_trx_contracts_rec.service_usage_tax_exist := 'N';
      p_xxd_okl_trx_contracts_rec.bef_contract_version := l_ver_num;
      p_xxd_okl_trx_contracts_rec.aft_contract_version := NULL;
      p_xxd_okl_trx_contracts_rec.rebook_resource := NULL;
      p_xxd_okl_trx_contracts_rec.completed_date := NULL;
      p_xxd_okl_trx_contracts_rec.bill_adjustment := NULL;
      p_xxd_okl_trx_contracts_rec.bill_vend_party_num := NULL;
      p_xxd_okl_trx_contracts_rec.bill_vend_party_site_num := NULL;
      p_xxd_okl_trx_contracts_rec.fund_vendor_id := NULL;
      p_xxd_okl_trx_contracts_rec.fund_vendor_site_id := NULL;
      p_xxd_okl_trx_contracts_rec.attribute_category := NULL;
      p_xxd_okl_trx_contracts_rec.attribute1 := NULL;
      p_xxd_okl_trx_contracts_rec.attribute2 := NULL;
      p_xxd_okl_trx_contracts_rec.attribute3 := NULL;
      p_xxd_okl_trx_contracts_rec.attribute4 := NULL;
      p_xxd_okl_trx_contracts_rec.attribute5 := NULL;
      p_xxd_okl_trx_contracts_rec.attribute6 := NULL;
      p_xxd_okl_trx_contracts_rec.attribute7 := NULL;
      p_xxd_okl_trx_contracts_rec.attribute8 := NULL;
      p_xxd_okl_trx_contracts_rec.attribute9 := NULL;
      p_xxd_okl_trx_contracts_rec.attribute10 := NULL;
      p_xxd_okl_trx_contracts_rec.attribute11 := NULL;
      p_xxd_okl_trx_contracts_rec.attribute12 := NULL;
      p_xxd_okl_trx_contracts_rec.attribute13 := NULL;
      p_xxd_okl_trx_contracts_rec.attribute14 := NULL;
      p_xxd_okl_trx_contracts_rec.attribute15 := NULL;
      p_xxd_okl_trx_contracts_rec.created_by := ln_user_id;
      p_xxd_okl_trx_contracts_rec.creation_date := SYSDATE;
      p_xxd_okl_trx_contracts_rec.last_updated_by := ln_user_id;
      p_xxd_okl_trx_contracts_rec.last_update_date := SYSDATE;
      p_xxd_okl_trx_contracts_rec.last_update_login := NULL;

      xxd_okl_trx_contracts_t_pk.insert_row (
         p_api_version                 => 1.0,
         p_init_msg_list               => okl_api.g_true,
         p_xxd_okl_trx_contracts_rec   => p_xxd_okl_trx_contracts_rec,
         x_return_status               => x_return_status,
         x_msg_count                   => ln_msg_count,
         x_msg_data                    => l_msg_data,
         x_xxd_okl_trx_contracts_rec   => x_xxd_okl_trx_contracts_rec);

      IF lc_return_status <> Fnd_Api.g_ret_sts_success
      THEN
         fnd_file.put_line (fnd_file.LOG,
                            'Rebook copy link creation is not successful');
         gc_error_message := 'Rebook copy link creation is not successful';

         IF ln_msg_count >= 1
         THEN
           <<err_loop>>
            FOR i IN 1 .. ln_msg_count
            LOOP
               gc_error_message :=
                  SUBSTR (
                     gc_error_message || ':' || Fnd_Msg_Pub.get (i, 'F'),
                     1,
                     1950);
            END LOOP err_loop;
         END IF;

         RAISE ex_rbk_processing;
      ELSE
         fnd_file.put_line (
            fnd_file.LOG,
               'The Rebook copy link created for contract '
            || ln_contract_id
            || ' is : '
            || x_xxd_okl_trx_contracts_rec.id);

         x_return_status := 'S';
         x_error_message := NULL;
      END IF;

      fnd_file.put_line (fnd_file.LOG,
                         'Updating okl_trx_requests status to COMPLETE');

      OPEN get_rebook_req_id_c (p_contract_id);

      FETCH get_rebook_req_id_c INTO l_trqv_rec;

      IF get_rebook_req_id_c%FOUND
      THEN
         l_trqv_rec.request_status_code := 'COMPLETE';
         okl_trx_requests_pub.update_trx_requests (
            p_api_version     => gn_api_version,
            p_init_msg_list   => xxd_api.gc_true,
            p_trqv_rec        => l_trqv_rec,
            x_return_status   => x_return_status,
            x_msg_data        => l_msg_data,
            x_msg_count       => ln_msg_count,
            x_trqv_rec        => lx_trqv_rec);

         IF lc_return_status <> Fnd_Api.g_ret_sts_success
         THEN
            fnd_file.put_line (
               fnd_file.LOG,
               'Update of okl_trx_requests status to COMPLETE is not successful');
            gc_error_message :=
               'Update of okl_trx_requests status to COMPLETE is not successful';

            IF ln_msg_count >= 1
            THEN
              <<err_loop>>
               FOR i IN 1 .. ln_msg_count
               LOOP
                  gc_error_message :=
                     SUBSTR (
                        gc_error_message || ':' || Fnd_Msg_Pub.get (i, 'F'),
                        1,
                        1950);
               END LOOP err_loop;
            END IF;

            RAISE ex_rbk_processing;
         ELSE
            fnd_file.put_line (
               fnd_file.LOG,
               'OKL_TRX_REQUESTS status updated to COMPLETE ');

            x_return_status := 'S';
            x_error_message := NULL;
         END IF;
      END IF;

      lp_x_request_details_rec := NULL;
      lp_x_request_details_rec.trq_id := x_request_id;
      lp_x_request_details_rec.bef_contract_version := l_ver_num;
      lp_x_request_details_rec.requested_by := ln_user_id;
      lp_x_request_details_rec.requested_date := SYSDATE;
      lp_x_request_details_rec.created_by := ln_user_id;
      lp_x_request_details_rec.creation_date := SYSDATE;
      lp_x_request_details_rec.last_updated_by := ln_user_id;
      lp_x_request_details_rec.last_update_date := SYSDATE;
      lp_x_request_details_rec.last_update_login := NULL;
      xxd_okl_trx_requests_t_pk.insert_row (
         p_api_version                => 1.0,
         p_init_msg_list              => xxd_api.gc_false,
         p_xxd_okl_trx_requests_rec   => lp_x_request_details_rec,
         x_return_status              => lc_return_status,
         x_msg_count                  => ln_msg_count,
         x_msg_data                   => l_msg_data,
         x_xxd_okl_trx_requests_rec   => lx_x_request_details_rec);


      IF lc_return_status <> Fnd_Api.g_ret_sts_success
      THEN
         fnd_file.put_line (
            fnd_file.LOG,
            'Insert into xxd_okl_trx_requests_t is not successful');
         gc_error_message :=
            'Insert into xxd_okl_trx_requests_t is not successful';

         IF ln_msg_count >= 1
         THEN
           <<err_loop>>
            FOR i IN 1 .. ln_msg_count
            LOOP
               gc_error_message :=
                  SUBSTR (
                     gc_error_message || ':' || Fnd_Msg_Pub.get (i, 'F'),
                     1,
                     1950);
            END LOOP err_loop;
         END IF;

         RAISE ex_rbk_processing;
      ELSE
         fnd_file.put_line (fnd_file.LOG,
                            'Insert into xxd_okl_trx_requests_t successful');

         x_return_status := 'S';
         x_error_message := NULL;
      END IF;

      lc_contract_status := 'REBOOKED';
      fnd_file.put_line (
         fnd_file.LOG,
         'current status of the rebook is - ' || lc_contract_status);

      fnd_file.put_line (
         fnd_file.LOG,
         'If the contract status is REBOOKED, updating the asset values');

      -- Updating asset values
      fnd_file.put_line (
         fnd_file.LOG,
            'Updating asset values in okl_txl_assets_b for the rebook chr_id-'
         || ln_rebook_chr_id);

      BEGIN
         xxd_upd_asset_values (p_contract_id,
                               ln_rebook_chr_id,
                               p_conc_req_id,
                               lc_ret_sts);

         IF (lc_ret_sts = 'E')
         THEN
            x_return_status := 'E';
            x_error_message := 'Update of asset values failed';
            fnd_file.put_line (fnd_file.LOG, x_error_message);
            RAISE ex_upd_processing;
         ELSE
            fnd_file.put_line (fnd_file.LOG,
                               'Update of Asset values successful');
            x_return_status := 'S';
            x_error_message := NULL;
         END IF;
      EXCEPTION
         WHEN OTHERS
         THEN
            x_return_status := 'E';
            x_error_message :=
               'Inside WHEN OTHERS exception - Error when updating asset values';
            fnd_file.put_line (fnd_file.LOG, x_error_message);
            RAISE ex_upd_processing;
      END;

      OPEN get_temp_dtls_c (p_contract_id, p_conc_req_id);

      FETCH get_temp_dtls_c INTO lcu_temp_dtls_rec;

      CLOSE get_temp_dtls_c;

      OPEN get_qa_id_c (ln_rebook_chr_id);

      FETCH get_qa_id_c INTO lcu_qa_id_rec;

      CLOSE get_qa_id_c;

      fnd_file.put_line (
         fnd_file.LOG,
            'Start QA validation process for rebook contract '
         || ln_rebook_chr_id);

      Okl_Contract_Book_Pub.execute_qa_check_list (
         p_api_version     => 1.0,
         p_init_msg_list   => Okc_Api.g_false,
         x_return_status   => lc_return_status,
         x_msg_count       => ln_msg_count,
         x_msg_data        => gc_error_message,
         p_qcl_id          => lcu_qa_id_rec.qcl_id,
         p_chr_id          => ln_rebook_chr_id,
         x_msg_tbl         => lc_msg_tbl);
         
   OPEN get_qa_id_c (ln_rebook_chr_id);

      FETCH get_qa_id_c INTO lcu_qa_id_rec;

      CLOSE get_qa_id_c;         

      IF lc_return_status <> Fnd_Api.g_ret_sts_success
      THEN
         fnd_file.put_line (fnd_file.LOG, 'QA validation not successful');
        --Start of modiifcation by Priya on 13-Feb-2017
          FOR ln_chklst_cntr IN lc_msg_tbl.FIRST.. lc_msg_tbl.LAST
          LOOP
             IF lc_msg_tbl (ln_chklst_cntr).error_status = 'E'
             THEN
                gc_error_message :=
                   substr(gc_error_message || '.' || lc_msg_tbl (ln_chklst_cntr).DATA,1,2000);
             END IF;
          END LOOP;                                                             

         x_error_message :=
            'Error during QA validation - ' || gc_error_message;
         RAISE ex_qa_processing;
      
      ELSIF lc_msg_tbl.COUNT > 0 AND lcu_qa_id_rec.status_code <> 'PASSED'
      THEN
      fnd_file.put_line (fnd_file.LOG, 'QA validation not successful');
          FOR ln_chklst_cntr IN lc_msg_tbl.FIRST.. lc_msg_tbl.LAST
          LOOP
             IF lc_msg_tbl (ln_chklst_cntr).error_status = 'E'
             THEN
                gc_error_message :=
                   substr(gc_error_message || '.' || lc_msg_tbl (ln_chklst_cntr).DATA,1,2000);
             END IF;
          END LOOP;   
          FOR ln_chklst_cntr IN lc_msg_tbl.FIRST.. lc_msg_tbl.LAST
          LOOP
             IF lc_msg_tbl (ln_chklst_cntr).error_status = 'E'
             THEN
                x_error_message :=
                    'Error during QA validation - ' || gc_error_message;
                RAISE ex_qa_processing;
             END IF;
          END LOOP;                                                          
         
      --End of modification by Priya on 13-Feb-2017   
      ELSE
         fnd_file.put_line (fnd_file.LOG, 'QA validation successful');
         x_return_status := 'S';
         x_error_message := NULL;
      END IF;

      lc_contract_status := 'PASSED';
      fnd_file.put_line (
         fnd_file.LOG,
         'current status of the rebook is - ' || lc_contract_status);

   

      fnd_file.put_line (
         fnd_file.LOG,
         'Generating streams for rebook contract ' || ln_rebook_chr_id);

      -- Generating the streams.

      IF (lcu_qa_id_rec.status_code = 'PASSED')
      THEN
         Okl_Generate_Streams_Pub.generate_streams (
            p_api_version           => 1.0,
            p_init_msg_list         => Okc_Api.g_false,
            p_khr_id                => ln_rebook_chr_id,
            p_generation_ctx_code   => 'AUTH',
            x_trx_number            => ln_trx_number,
            x_trx_status            => lc_trx_status,
            x_return_status         => lc_return_status,
            x_msg_count             => ln_msg_count,
            x_msg_data              => gc_error_message);
      -- Committing if successful
      END IF;

      IF lc_return_status <> Fnd_Api.g_ret_sts_success
      THEN
         fnd_file.put_line (fnd_file.LOG, 'Stream Generation not successful');

         IF ln_msg_count >= 1
         THEN
           <<err_loop>>
            FOR i IN 1 .. ln_msg_count
            LOOP
               gc_error_message :=
                  SUBSTR (
                     gc_error_message || ':' || Fnd_Msg_Pub.get (i, 'F'),
                     1,
                     2000);
            END LOOP err_loop;
         END IF;

         x_error_message :=
            'Error during Stream Generation - ' || gc_error_message;
         RAISE ex_strm_processing;
      ELSE
         COMMIT;
         fnd_file.put_line (fnd_file.LOG, 'Stream Generation successful');
         x_return_status := 'S';
         x_error_message := NULL;
      END IF;

      ln_interval := 15;
      --NVL (Fnd_Profile.VALUE ('XXD_CONTRACT_STATUS_CHECK_TIME'), 60);
      ln_loop_cnt := 0;
      ln_timeout := p_time_out;

     --NVL (Fnd_Profile.VALUE ('OKL_STREAMS_TIME_OUT'), 0);

     <<chk_status>>
      LOOP
         DBMS_LOCK.sleep (ln_interval);

         OPEN get_qa_id_c (ln_rebook_chr_id);

         FETCH get_qa_id_c INTO lcu_qa_id_rec;

         IF UPPER (lcu_qa_id_rec.status_code) = 'COMPLETE'
         THEN
            CLOSE get_qa_id_c;

            EXIT chk_status;
         END IF;

         CLOSE get_qa_id_c;

         ln_loop_cnt := ln_loop_cnt + ln_interval;

         IF (ln_loop_cnt / ln_interval > ln_timeout)
         THEN
            fnd_file.put_line (fnd_file.LOG, 'Timeout!!');
            Fnd_Message.set_name ('XXD', 'XXD_OKL_STRMGEN_TIMEOUT_700815');
            Fnd_Message.set_token ('REQ_VAL', ln_loop_cnt / ln_interval);
            gc_error_message := Fnd_Message.get;

            x_error_message := 'Time out error - ' || gc_error_message;
            RAISE ex_strm_processing;
         END IF;

         COMMIT;
      END LOOP chk_status;

      lc_contract_status := 'COMPLETE';
      fnd_file.put_line (
         fnd_file.LOG,
         'current status of the rebook is - ' || lc_contract_status);

      Okl_Contract_Book_Pub.submit_for_approval (
         p_api_version     => 1.0,
         p_init_msg_list   => Okc_Api.g_false,
         x_return_status   => lc_return_status,
         x_msg_count       => ln_msg_count,
         x_msg_data        => gc_error_message,
         p_chr_id          => ln_rebook_chr_id);

      IF lc_return_status <> Fnd_Api.g_ret_sts_success
      THEN
         IF ln_msg_count >= 1
         THEN
           <<err_loop>>
            FOR i IN 1 .. ln_msg_count
            LOOP
               gc_error_message :=
                  SUBSTR (
                     gc_error_message || ':' || Fnd_Msg_Pub.get (i, 'F'),
                     1,
                     2000);
            END LOOP err_loop;
         END IF;

         x_error_message := 'Error in approval - ' || gc_error_message;
         RAISE ex_appr_processing;
      END IF;


     <<approval>>
      LOOP
         OPEN get_qa_id_c (ln_rebook_chr_id);

         FETCH get_qa_id_c INTO lcu_qa_id_rec;

         CLOSE get_qa_id_c;

         IF    UPPER (lcu_qa_id_rec.status_code) = 'APPROVED'
            OR ln_time_out > 60
         THEN
            EXIT approval;
         ELSE
            DBMS_LOCK.sleep (10);
            ln_time_out := ln_time_out + 10;
         END IF;
      END LOOP approval;


      OPEN get_qa_id_c (ln_rebook_chr_id);

      FETCH get_qa_id_c INTO lcu_qa_id_rec;

      CLOSE get_qa_id_c;


      lc_contract_status := 'APPROVED';
      fnd_file.put_line (fnd_file.LOG, 'Contract is Approved');
      Okl_Contract_Book_Pub.activate_contract (
         p_api_version     => 1.0,
         p_init_msg_list   => Okc_Api.g_false,
         x_return_status   => lc_return_status,
         x_msg_count       => ln_msg_count,
         x_msg_data        => gc_error_message,
         p_chr_id          => ln_rebook_chr_id);


      IF lc_return_status <> Fnd_Api.g_ret_sts_success
      THEN
         gc_error_message := 'Error While Abandoning the Contract';

         IF ln_msg_count >= 1
         THEN
           <<err_loop>>
            FOR i IN 1 .. ln_msg_count
            LOOP
               gc_error_message :=
                  (gc_error_message || ':' || Fnd_Msg_Pub.get (i, 'F'));
            END LOOP err_loop;

            x_error_message := 'Error in activation -' || gc_error_message;
            RAISE ex_act_processing;
         END IF;
      END IF;

      fnd_file.put_line (fnd_file.LOG, 'Contract is Activated');
      lc_contract_status := 'BOOKED';
      lc_process_status := 'SUCCESS';

      -- Updating temp table with rebook success status
      fnd_file.put_line (
         fnd_file.LOG,
         'Updating status after rebook is complete in the temp table');
      lcu_temp_dtls_rec.status := 'R';
      xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);

      Fnd_File.put_line (
         Fnd_File.LOG,
            'Contract with CONTRACT ID = '
         || p_contract_id
         || ' : '
         || 'has been successfully rebooked');


      Fnd_File.put_line (
         Fnd_File.LOG,
         'Checking whether the contract is a Non-Accrual contract');

      OPEN get_hdr_lkp_c;

      FETCH get_hdr_lkp_c INTO ln_hdr_lkp_code;

      CLOSE get_hdr_lkp_c;

      OPEN get_lin_lkp_c;

      FETCH get_lin_lkp_c INTO ln_lin_lkp_code;

      CLOSE get_lin_lkp_c;

      OKL_GENERATE_ACCRUALS_PVT.VALIDATE_ACCRUAL_RULE (
         x_return_status   => lc_non_acc_status,
         x_result          => l_rule_result,
         p_ctr_id          => p_contract_id);

      IF (lc_non_acc_status <> Okl_Api.G_RET_STS_SUCCESS)
      THEN
         Okl_Api.set_message (
            p_app_name       => g_app_name,
            p_msg_name       => 'OKL_AGN_RULE_VALD_ERROR',
            p_token1         => g_contract_number_token,
            p_token1_value   => lcu_temp_dtls_rec.contract_number);

         RAISE Okl_Api.G_EXCEPTION_UNEXPECTED_ERROR;
      END IF;

      IF l_rule_result = 'N'
      THEN
         fnd_file.put_line (fnd_file.LOG,
                            'The contract is a Non-Accrual Contract');


         --open cursor for non-accrual for the contract
         --loop
         FOR lc_non_accrual_rec IN get_non_accrual_c (p_contract_id)
         LOOP
            EXIT WHEN get_non_accrual_c%NOTFOUND;
            l_tcnv_rec.id := lc_non_accrual_rec.id;
            --l_tcnv_rec.object_version_number :=10;
            l_tcnv_rec.tcn_type := ln_hdr_lkp_code;

            fnd_file.put_line (
               fnd_file.LOG,
               'calling API for update the okl_trx_contracts');
            okl_trx_contracts_pvt.update_trx_contracts (
               p_api_version     => 1.0,
               p_init_msg_list   => okl_api.g_false,
               x_return_status   => lcn_ret_sts,
               x_msg_count       => ln_msg_count,
               x_msg_data        => lc_msg_data,
               p_tcnv_rec        => l_tcnv_rec,
               x_tcnv_rec        => lcx_tcnv_rec);

            COMMIT;

            IF lcn_ret_sts <> Fnd_Api.g_ret_sts_success
            THEN
               gc_error_message :=
                  'Error While updating the okl_trx_Contracts';

               IF ln_msg_count >= 1
               THEN
                 <<err_loop>>
                  FOR i IN 1 .. ln_msg_count
                  LOOP
                     gc_error_message :=
                        (gc_error_message || ':' || Fnd_Msg_Pub.get (i, 'F'));
                  END LOOP err_loop;

                  x_error_message :=
                        'Error While updating the okl_trx_Contracts -'
                     || gc_error_message;
               END IF;

               lcu_temp_dtls_rec.error_msg :=
                  lcu_temp_dtls_rec.error_msg || x_error_message;
               xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);
            ELSE
               fnd_file.put_line (
                  fnd_file.LOG,
                  'Header update sucessful, Updating the lines table');

               FOR lc_non_accrual_lines_rec
                  IN get_non_accrual_lines_c (p_contract_id,
                                              lc_non_accrual_rec.id)
               LOOP
                  EXIT WHEN get_non_accrual_lines_c%NOTFOUND;
                  l_tclv_rec.id := lc_non_accrual_lines_rec.id;
                  --l_tclv_rec.object_version_number :=10;
                  l_tclv_rec.tcl_type := ln_lin_lkp_code;
                  --Start of modification by Priya on 09-Feb-2017
                  l_tclv_rec.last_update_date := TO_DATE('31-DEC-2015','DD-MON-RRRR') ;
                  --End of modification by Priya on 09-Feb-2017


                  fnd_file.put_line (
                     fnd_file.LOG,
                     'Calling API for update the okl_trx_contracts_lines table');
                  okl_trx_contracts_pvt.update_trx_cntrct_lines (
                     p_api_version     => 1.0,
                     p_init_msg_list   => okl_api.g_false,
                     x_return_status   => lcv_ret_sts,
                     x_msg_count       => ln_msg_count,
                     x_msg_data        => lc_msg_data,
                     p_tclv_rec        => l_tclv_rec,
                     x_tclv_rec        => lcx_tclv_rec);

                  COMMIT;

                  IF lcv_ret_sts <> Fnd_Api.g_ret_sts_success
                  THEN
                     gc_error_message :=
                        'Error While updating the okl_trx_Contracts';

                     IF ln_msg_count >= 1
                     THEN
                       <<err_loop>>
                        FOR i IN 1 .. ln_msg_count
                        LOOP
                           gc_error_message :=
                              (   gc_error_message
                               || ':'
                               || Fnd_Msg_Pub.get (i, 'F'));
                        END LOOP err_loop;

                        x_error_message :=
                              'Error While updating the okl_trx_Contracts -'
                           || gc_error_message;
                     END IF;

                     lcu_temp_dtls_rec.error_msg :=
                        lcu_temp_dtls_rec.error_msg || x_error_message;
                     xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);
                  END IF;
               END LOOP;
            END IF;
         END LOOP;
   --Start of modification by Priya on 05-Jan-2017 for OPLR
         FOR dist_rec IN dist_csr (p_contract_id)
         LOOP
            i := i + 1;
            l_tabv_tbl_in (i).ID := dist_rec.ID;
            l_tabv_tbl_in (i).post_to_gl := 'N';
         END LOOP;

         fnd_file.put_line (fnd_file.LOG,
                            'l_tabv_tbl_in.count : ' || l_tabv_tbl_in.COUNT);

         IF (l_tabv_tbl_in.COUNT > 0)
         THEN
            okl_trns_acc_dstrs_pub.update_trns_acc_dstrs (
               p_api_version     => '1.0',
               p_init_msg_list   => OKC_API.g_false,
               x_return_status   => lc_return_status,
               x_msg_count       => ln_msg_count,
               x_msg_data        => lc_msg_data,
               p_tabv_tbl        => l_tabv_tbl_in,
               x_tabv_tbl        => l_tabv_tbl_out);



            IF lcv_ret_sts <> Fnd_Api.g_ret_sts_success
            THEN
               gc_error_message :=
                  'Error While updating the okl_trns_acct_dstrs';

               IF ln_msg_count >= 1
               THEN
                 <<err_loop>>
                  FOR i IN 1 .. ln_msg_count
                  LOOP
                     gc_error_message :=
                        (gc_error_message || ':' || Fnd_Msg_Pub.get (i, 'F'));
                  END LOOP err_loop;

                  x_error_message :=
                        'Error While updating the okl_trns_acct_dstrs -'
                     || gc_error_message;
               END IF;

               lcu_temp_dtls_rec.error_msg :=
                  lcu_temp_dtls_rec.error_msg || x_error_message;
               xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);
            END IF;
         END IF;
   --End of modification by Priya on 05-Jan-2017 for OPLR
      ELSE
         fnd_file.put_line (fnd_file.LOG,
                            'The contract is not a Non-Accrual Contract');
      END IF;

      fnd_file.put_line (fnd_file.LOG, 'Updating depreciation flag');
      change_depreciation_flag_prc (p_contract_id,
                                    ln_rebook_chr_id,
                                    lc_return_status,
                                    lc_msg_data);

      IF (lc_return_status = 'S')
      THEN
         fnd_file.put_line (
            fnd_file.LOG,
            'FA update succesfully completed - calling the cta adjustment procedure');
         create_fa_adj_cta_asst_prc (p_contract_id,
                                     lc_return_status,
                                     lc_msg_data);

         --if successful, proceeding with cta adjustment
         IF (lc_return_status = 'S')
         THEN
            fnd_file.put_line (
               fnd_file.LOG,
               'FA adjustment for CTA assets succesfully completed');

            x_return_status := 'S';
            lcu_temp_dtls_rec.status := 'S';
            lcu_temp_dtls_rec.processed_flag := 'Y';
            xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);
         ELSE
            x_return_status := 'E';
            fnd_file.put_line (fnd_file.LOG,
                               'FA adjustment for CTA assets not successful'); -- no update to the status of the contract, it will still be R-N
            lcu_temp_dtls_rec.error_msg :=
                  lcu_temp_dtls_rec.error_msg
               || 'FA adjustment for CTA assets not successful';
            xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);
         END IF;
      --if FA update is not succesful, we are not doing CTA either
      ELSE
         fnd_file.put_line (fnd_file.LOG, 'FA update not successful');
         lcu_temp_dtls_rec.error_msg := 'FA update not successful';
         xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);
      END IF;


      Fnd_File.put_line (
         Fnd_File.LOG,
            'Contract with CONTRACT ID = '
         || p_contract_id
         || 'was rebooked succesfully and FA update completed');

      fnd_file.put_line (fnd_file.LOG, 'Inserting note for the contract');

      --Note Creation
      jtf_notes_pub.create_note (
         p_api_version          => 1,
         x_return_status        => lc_jtf_ret_sts,
         x_msg_count            => ln_jtf_msg_cnt,
         x_msg_data             => lc_jtf_msg_data,
         p_source_object_id     => p_contract_id,
         p_source_object_code   => 'OKC_K_HEADER',
         p_notes                =>    'The contract was rebooked by '
                                   || ln_user_name
                                   || ln_notes_created
                                   || ' on '
                                   || SYSDATE,
         p_note_status          => 'I',
         p_entered_by           => ln_user_id,
         p_entered_date         => SYSDATE,
         x_jtf_note_id          => ln_jtf_note_id,
         p_last_update_date     => SYSDATE,
         p_last_updated_by      => ln_user_id,
         p_creation_date        => SYSDATE,
         p_created_by           => ln_user_id,
         p_last_update_login    => ln_user_id,
         p_note_type            => 'AS_SYSTEM');

      IF (lc_jtf_ret_sts <> fnd_api.g_ret_sts_success)
      THEN
         fnd_file.put_line (
            fnd_file.LOG,
            'Error in note creation for contract ' || p_contract_id);
      ELSE
         fnd_file.put_line (fnd_file.LOG,
                            'Note created for the contract successfully');
      END IF;

      -- Updating the temp table with the reversal values

      fnd_file.put_line (
         fnd_file.LOG,
         'Calling the procedure to update temp table with accounting details');

      xxd_upd_rev_dtls_prc (p_contract_id, p_conc_req_id, lc_rev_status);

      IF (lc_rev_status = 'E')
      THEN
         fnd_file.put_line (
            fnd_file.LOG,
            ' The reversal values update in xxd_ol_rbk_temp_t was not succesful');
         x_return_status := 'W';
         x_error_message := 'Reversal values update in the temp table failed';

         IF (lcu_temp_dtls_rec.status = 'S')
         THEN
            lcu_temp_dtls_rec.status := 'W';
            lcu_temp_dtls_rec.error_msg := 'Update of reveral values failed';
            xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);
         ELSE
            lcu_temp_dtls_rec.error_msg :=
                  lcu_temp_dtls_rec.error_msg
               || 'Update of reveral values failed';
            xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);
         END IF;
      ELSE
         fnd_file.put_line (
            fnd_file.LOG,
            ' The reversal values update in xxd_ol_rbk_temp_t is completed');
         x_error_message := 'Reversal values update in the temp table is done';
      END IF;

      lc_dummy_stat := lcu_temp_dtls_rec.status;
      lc_dummy_err_msg := lcu_temp_dtls_rec.error_msg;
      lc_return_status := NULL;
      lcu_temp_dtls_rec := NULL;

      OPEN get_temp_dtls_c (p_contract_id, p_conc_req_id);

      FETCH get_temp_dtls_c INTO lcu_temp_dtls_rec;

      CLOSE get_temp_dtls_c;

      --Fetching accrual data

      OPEN get_rental_accrual_c (p_contract_id);

      FETCH get_rental_accrual_c INTO ln_rental_accrual;

      CLOSE get_rental_accrual_c;

      OPEN get_rental_non_acc_c (p_contract_id);

      FETCH get_rental_non_acc_c INTO ln_rental_non_accrual;

      CLOSE get_rental_non_acc_c;

      OPEN get_total_accrual_c (p_contract_id);

      FETCH get_total_accrual_c INTO ln_tot_accrual;

      CLOSE get_total_accrual_c;

      fnd_file.put_line (
         fnd_file.LOG,
            'Rental Accrual = '
         || ln_rental_accrual
         || '   Non Accrual = '
         || ln_rental_non_accrual
         || '   Total Accrual = '
         || ln_tot_Accrual);

      lcu_temp_dtls_rec.accrual_rent_inc := ln_rental_accrual;
      lcu_temp_dtls_rec.non_accrual_tot := ln_rental_non_accrual;
      lcu_temp_dtls_rec.total_accrual := ln_tot_accrual;

      xxd_upd_accum_deprn (p_contract_id,
                           p_conc_req_id,
                           p_accum_deprn,
                           lc_return_status);

      IF (lc_return_status = 'S')
      THEN
         fnd_file.put_line (
            fnd_file.LOG,
            ' Accumulated Depreciation update successfully completed');

         lcu_temp_dtls_rec.accum_deprn := p_accum_deprn;
      ELSE
         fnd_file.put_line (
            fnd_file.LOG,
            ' Accumulated Depreciation update not successful');

         IF (lc_dummy_stat = 'S')
         THEN
            lcu_temp_dtls_rec.status := 'W';
            lcu_temp_dtls_rec.error_msg :=
                  lc_dummy_err_msg
               || 'Contract failed in updating accumulated depreciation';
            xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);
         ELSE
            lcu_temp_dtls_rec.error_msg :=
                  lc_dummy_err_msg
               || 'Contract failed in updating accumulated depreciation';
         END IF;
      END IF;

      fnd_file.put_line (
         fnd_file.LOG,
         'Updating the Accrual and Accumulated Depreciation values in the temp table');

      xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);

      fnd_file.put_line (
         fnd_file.LOG,
         'End of the Rebook Procedure, Returning to Child Procedure');
   EXCEPTION
      WHEN ex_req_processing
      THEN
         fnd_file.put_line (
            fnd_file.LOG,
            'Exception - Rebook procedure failed in creation of rebook request');

         IF get_qa_id_c%ISOPEN
         THEN
            CLOSE get_qa_id_c;
         END IF;

         ROLLBACK;

         UPDATE xxd_ol_rbk_temp_t
            SET error_msg =
                      'Error in rebook request creation '
                   || SUBSTR (gc_error_message, 1, 3000),
                status = 'E',
                processed_flag = 'N'
          WHERE contract_id = p_contract_id AND request_id = p_conc_req_id;

         COMMIT;

         fnd_file.put_line (fnd_file.LOG,
                            'Reverting product update and tax owner update');

         OPEN get_orig_details (p_contract_id, p_conc_req_id);

         FETCH get_orig_details
         INTO lc_orig_product_name, ln_orig_pdt_id;

         CLOSE get_orig_details;

         IF lc_orig_product_name LIKE '%NON%TAX%'
         THEN
            UPDATE okc_rules_b
               SET RULE_INFORMATION1 = 'LESSEE'
             WHERE     dnz_chr_id = p_contract_id
                   AND rule_information_category = 'LATOWN';
         END IF;

         UPDATE okl_k_headers
            SET pdt_id = ln_orig_pdt_id, deal_type = 'LEASEDF'
          WHERE id = p_contract_id;

         COMMIT;
         x_return_status := 'E';
         x_error_message :=
            SUBSTR (
                  '.Rebook procedure failed during creation of rebook request :'
               || gc_error_message,
               1,
               1000);
      WHEN ex_rbk_processing
      THEN
         fnd_file.put_line (
            fnd_file.LOG,
            'Exception - Rebook procedure failed in creation of rebook copy');

         IF get_qa_id_c%ISOPEN
         THEN
            CLOSE get_qa_id_c;
         END IF;

         ROLLBACK;

         x_return_status := 'E';
         fnd_file.put_line (
            fnd_file.LOG,
               ' Updating rebook error message in the temp table for contract '
            || p_contract_id
            || ' and request id '
            || p_conc_req_id);

         UPDATE xxd_ol_rbk_temp_t
            SET error_msg =
                      'Error during creation of rebook copy'
                   || SUBSTR (gc_error_message, 1, 3000),
                processed_flag = 'N',
                status = 'E'
          WHERE contract_id = p_contract_id AND request_id = p_conc_req_id;

         COMMIT;
         fnd_file.put_line (fnd_file.LOG,
                            'Reverting product update and tax owner update');

         OPEN get_orig_details (p_contract_id, p_conc_req_id);

         FETCH get_orig_details
         INTO lc_orig_product_name, ln_orig_pdt_id;

         CLOSE get_orig_details;

         IF lc_orig_product_name LIKE '%NON%TAX%'
         THEN
            UPDATE okc_rules_b
               SET RULE_INFORMATION1 = 'LESSEE'
             WHERE     dnz_chr_id = p_contract_id
                   AND rule_information_category = 'LATOWN';
         END IF;

         UPDATE okl_k_headers
            SET pdt_id = ln_orig_pdt_id, deal_type = 'LEASEDF'
          WHERE id = p_contract_id;

         COMMIT;
         x_return_status := 'E';
         x_error_message :=
            SUBSTR (
                  '.Rebook procedure failed during creation of rebook copy :'
               || gc_error_message,
               1,
               1000);
      WHEN ex_upd_processing
      THEN
         IF get_qa_id_c%ISOPEN
         THEN
            CLOSE get_qa_id_c;
         END IF;

         fnd_file.put_line (
            fnd_file.LOG,
            'Exception - Update of asset values failed' || x_error_message);

         UPDATE xxd_ol_rbk_temp_t
            SET processed_flag = 'N',
                status = 'E',
                error_msg = 'Error during update of asset values'
          WHERE contract_id = p_contract_id AND request_id = p_conc_req_id;

         COMMIT;

         fnd_file.put_line (fnd_file.LOG,
                            'Reverting product update and tax owner update');

         OPEN get_orig_details (p_contract_id, p_conc_req_id);

         FETCH get_orig_details
         INTO lc_orig_product_name, ln_orig_pdt_id;

         CLOSE get_orig_details;

         IF lc_orig_product_name LIKE '%NON%TAX%'
         THEN
            UPDATE okc_rules_b
               SET RULE_INFORMATION1 = 'LESSEE'
             WHERE     dnz_chr_id = p_contract_id
                   AND rule_information_category = 'LATOWN';
         END IF;

         UPDATE okl_k_headers
            SET pdt_id = ln_orig_pdt_id, deal_type = 'LEASEDF'
          WHERE id = p_contract_id;


         COMMIT;
         x_return_status := 'E';

         fnd_file.put_line (fnd_file.LOG, 'Calling the abandon procedure: ');
         lc_err_msg := gc_error_message;
         lt_rev_tbl (1).chr_id := ln_rebook_chr_id;
         okl_transaction_pub.abandon_revisions (
            p_api_version       => 1.0,
            p_init_msg_list     => xxd_api.gc_true,
            x_return_status     => gc_return_status,
            x_msg_count         => gn_msg_count,
            x_msg_data          => gc_error_message,
            p_rev_tbl           => lt_rev_tbl,
            p_contract_status   => 'ABANDONED',
            p_tsu_code          => 'CANCELED');

         IF gc_return_status <> fnd_api.g_ret_sts_success
         THEN
            IF gn_msg_count > 0
            THEN
              <<err_loop>>
               FOR i IN 1 .. gn_msg_count
               LOOP
                  gc_error_message :=
                     SUBSTR (
                        gc_error_message || ':' || fnd_msg_pub.get (i, 'F'),
                        1,
                        2000);
               END LOOP err_loop;
            END IF;

            x_error_message :=
               x_error_message || lc_err_msg || gc_error_message;
            fnd_file.put_line (fnd_file.LOG, 'Exception -Abandon failedl');
            x_return_status := 'W';

            lcu_temp_dtls_rec.error_msg :=
                  'Update asset values in the table errored out'
               || x_error_message
               || '- Abandon failed';
            lcu_temp_dtls_rec.status := 'E';
            lcu_temp_dtls_rec.processed_flag := 'N';
            xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);

            COMMIT;
         END IF;

         IF (x_return_status = 'W')
         THEN
            x_error_message :=
               'Error during Abandoning rebook copy - ' || gc_error_message;
            x_return_status := 'E';
         ELSE
            fnd_file.put_line (fnd_file.LOG, 'Abandoned rebook copy');
            x_return_status := 'E';
            x_error_message :=
               'Rebook failed - abandoned the rebook copy and reverted product/tax owner updates ';

            fnd_file.put_line (fnd_file.LOG, x_error_message);
         END IF;
      WHEN ex_qa_processing
      THEN
         fnd_file.put_line (
            fnd_file.LOG,
            'Exception - Error in QA validation' || x_error_message);

         lcu_temp_dtls_rec.error_msg :=
            'Error in QA validation - ' || x_error_message;
         lcu_temp_dtls_rec.status := 'E';
         lcu_temp_dtls_rec.processed_flag := 'N';
         xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);

         IF get_qa_id_c%ISOPEN
         THEN
            CLOSE get_qa_id_c;
         END IF;

         fnd_file.put_line (fnd_file.LOG,
                            'Reverting product update and tax owner update');

         OPEN get_orig_details (p_contract_id, p_conc_req_id);

         FETCH get_orig_details
         INTO lc_orig_product_name, ln_orig_pdt_id;

         CLOSE get_orig_details;

         IF lc_orig_product_name LIKE '%NON%TAX%'
         THEN
            UPDATE okc_rules_b
               SET RULE_INFORMATION1 = 'LESSEE'
             WHERE     dnz_chr_id = p_contract_id
                   AND rule_information_category = 'LATOWN';
         END IF;

         UPDATE okl_k_headers
            SET pdt_id = ln_orig_pdt_id, deal_type = 'LEASEDF'
          WHERE id = p_contract_id;

         x_return_status := 'E';

         COMMIT;

         fnd_file.put_line (fnd_file.LOG, 'Calling the abandon procedure: ');
         lc_err_msg := gc_error_message;
         lt_rev_tbl (1).chr_id := ln_rebook_chr_id;
         okl_transaction_pub.abandon_revisions (
            p_api_version       => 1.0,
            p_init_msg_list     => xxd_api.gc_true,
            x_return_status     => gc_return_status,
            x_msg_count         => gn_msg_count,
            x_msg_data          => gc_error_message,
            p_rev_tbl           => lt_rev_tbl,
            p_contract_status   => 'ABANDONED',
            p_tsu_code          => 'CANCELED');

         IF gc_return_status <> fnd_api.g_ret_sts_success
         THEN
            IF gn_msg_count > 0
            THEN
              <<err_loop>>
               FOR i IN 1 .. gn_msg_count
               LOOP
                  gc_error_message :=
                     SUBSTR (
                        gc_error_message || ':' || fnd_msg_pub.get (i, 'F'),
                        1,
                        2000);
               END LOOP err_loop;
            END IF;

            x_error_message :=
               x_error_message || lc_err_msg || gc_error_message;
            fnd_file.put_line (fnd_file.LOG, 'Exception -Abandon failedl');
            x_return_status := 'W';

            lcu_temp_dtls_rec.error_msg :=
                  'QA validation failed'
               || x_error_message
               || '- Abandon failed';
            lcu_temp_dtls_rec.status := 'E';
            lcu_temp_dtls_rec.processed_flag := 'N';
            xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);
         END IF;

         IF (x_return_status = 'W')
         THEN
            x_error_message :=
               'Error during Abandoning rebook copy - ' || gc_error_message;
            x_return_status := 'E';
         ELSE
            fnd_file.put_line (fnd_file.LOG, 'Abandoned rebook copy');
            x_return_status := 'E';
            x_error_message :=
               'Rebook failed - abandoned the rebook copy and reverted product/tax owner updates ';

            fnd_file.put_line (fnd_file.LOG, x_error_message);
         END IF;
      WHEN ex_strm_processing
      THEN
         fnd_file.put_line (
            fnd_file.LOG,
            'Exception - Error in Stream Generation' || x_error_message);

         lcu_temp_dtls_rec.error_msg :=
            'Error in Stream Generation - ' || x_error_message;
         lcu_temp_dtls_rec.status := 'E';
         lcu_temp_dtls_rec.processed_flag := 'N';
         xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);

         IF get_qa_id_c%ISOPEN
         THEN
            CLOSE get_qa_id_c;
         END IF;


         fnd_file.put_line (fnd_file.LOG,
                            'Reverting product update and tax owner update');

         OPEN get_orig_details (p_contract_id, p_conc_req_id);

         FETCH get_orig_details
         INTO lc_orig_product_name, ln_orig_pdt_id;

         CLOSE get_orig_details;

         IF lc_orig_product_name LIKE '%NON%TAX%'
         THEN
            UPDATE okc_rules_b
               SET RULE_INFORMATION1 = 'LESSEE'
             WHERE     dnz_chr_id = p_contract_id
                   AND rule_information_category = 'LATOWN';
         END IF;

         UPDATE okl_k_headers
            SET pdt_id = ln_orig_pdt_id, deal_type = 'LEASEDF'
          WHERE id = p_contract_id;

         x_return_status := 'E';

         COMMIT;

         fnd_file.put_line (fnd_file.LOG, 'Calling the abandon procedure: ');
         lc_err_msg := gc_error_message;
         lt_rev_tbl (1).chr_id := ln_rebook_chr_id;
         okl_transaction_pub.abandon_revisions (
            p_api_version       => 1.0,
            p_init_msg_list     => xxd_api.gc_true,
            x_return_status     => gc_return_status,
            x_msg_count         => gn_msg_count,
            x_msg_data          => gc_error_message,
            p_rev_tbl           => lt_rev_tbl,
            p_contract_status   => 'ABANDONED',
            p_tsu_code          => 'CANCELED');

         IF gc_return_status <> fnd_api.g_ret_sts_success
         THEN
            IF gn_msg_count > 0
            THEN
              <<err_loop>>
               FOR i IN 1 .. gn_msg_count
               LOOP
                  gc_error_message :=
                     SUBSTR (
                        gc_error_message || ':' || fnd_msg_pub.get (i, 'F'),
                        1,
                        2000);
               END LOOP err_loop;
            END IF;

            x_error_message :=
               x_error_message || lc_err_msg || gc_error_message;
            fnd_file.put_line (fnd_file.LOG, 'Exception -Abandon failedl');
            x_return_status := 'W';

            lcu_temp_dtls_rec.error_msg :=
                  'Stream Generation failed'
               || x_error_message
               || '- Abandon failed';
            lcu_temp_dtls_rec.status := 'E';
            lcu_temp_dtls_rec.processed_flag := 'N';
            xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);
         END IF;

         IF (x_return_status = 'W')
         THEN
            x_error_message :=
               'Error during Abandoning rebook copy - ' || gc_error_message;
            x_return_status := 'E';
         ELSE
            fnd_file.put_line (fnd_file.LOG, 'Abandoned rebook copy');
            x_return_status := 'E';
            x_error_message :=
               'Rebook failed - abandoned the rebook copy and reverted product/tax owner updates ';

            fnd_file.put_line (fnd_file.LOG, x_error_message);
         END IF;
      WHEN ex_appr_processing
      THEN
         fnd_file.put_line (
            fnd_file.LOG,
            'Exception - Error in Contract Approval' || x_error_message);

         lcu_temp_dtls_rec.error_msg :=
            'Error in Contract Approval - ' || x_error_message;
         lcu_temp_dtls_rec.status := 'E';
         lcu_temp_dtls_rec.processed_flag := 'N';
         xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);

         IF get_qa_id_c%ISOPEN
         THEN
            CLOSE get_qa_id_c;
         END IF;

         fnd_file.put_line (fnd_file.LOG,
                            'Reverting product update and tax owner update');

         OPEN get_orig_details (p_contract_id, p_conc_req_id);

         FETCH get_orig_details
         INTO lc_orig_product_name, ln_orig_pdt_id;

         CLOSE get_orig_details;

         IF lc_orig_product_name LIKE '%NON%TAX%'
         THEN
            UPDATE okc_rules_b
               SET RULE_INFORMATION1 = 'LESSEE'
             WHERE     dnz_chr_id = p_contract_id
                   AND rule_information_category = 'LATOWN';
         END IF;

         UPDATE okl_k_headers
            SET pdt_id = ln_orig_pdt_id, deal_type = 'LEASEDF'
          WHERE id = p_contract_id;

         x_return_status := 'E';

         COMMIT;

         fnd_file.put_line (fnd_file.LOG, 'Calling the abandon procedure: ');
         lc_err_msg := gc_error_message;
         lt_rev_tbl (1).chr_id := ln_rebook_chr_id;
         okl_transaction_pub.abandon_revisions (
            p_api_version       => 1.0,
            p_init_msg_list     => xxd_api.gc_true,
            x_return_status     => gc_return_status,
            x_msg_count         => gn_msg_count,
            x_msg_data          => gc_error_message,
            p_rev_tbl           => lt_rev_tbl,
            p_contract_status   => 'ABANDONED',
            p_tsu_code          => 'CANCELED');

         IF gc_return_status <> fnd_api.g_ret_sts_success
         THEN
            IF gn_msg_count > 0
            THEN
              <<err_loop>>
               FOR i IN 1 .. gn_msg_count
               LOOP
                  gc_error_message :=
                     SUBSTR (
                        gc_error_message || ':' || fnd_msg_pub.get (i, 'F'),
                        1,
                        2000);
               END LOOP err_loop;
            END IF;

            x_error_message :=
               x_error_message || lc_err_msg || gc_error_message;
            fnd_file.put_line (fnd_file.LOG, 'Exception -Abandon failedl');
            x_return_status := 'W';

            lcu_temp_dtls_rec.error_msg :=
                  'Contract Approval Failed'
               || x_error_message
               || '- Abandon failed';
            lcu_temp_dtls_rec.status := 'E';
            lcu_temp_dtls_rec.processed_flag := 'N';
            xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);
         END IF;

         IF (x_return_status = 'W')
         THEN
            x_error_message :=
               'Error during Abandoning rebook copy - ' || gc_error_message;
            x_return_status := 'E';
         ELSE
            fnd_file.put_line (fnd_file.LOG, 'Abandoned rebook copy');
            x_return_status := 'E';
            x_error_message :=
               'Rebook failed - abandoned the rebook copy and reverted product/tax owner updates ';

            fnd_file.put_line (fnd_file.LOG, x_error_message);
         END IF;
      WHEN ex_act_processing
      THEN
         fnd_file.put_line (
            fnd_file.LOG,
            'Exception - Error in Activation' || x_error_message);

         lcu_temp_dtls_rec.error_msg :=
            'Error in Activation - ' || x_error_message;
         lcu_temp_dtls_rec.status := 'E';
         lcu_temp_dtls_rec.processed_flag := 'N';
         xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);


         IF get_qa_id_c%ISOPEN
         THEN
            CLOSE get_qa_id_c;
         END IF;

         --         COMMIT;

         fnd_file.put_line (fnd_file.LOG,
                            'Reverting product update and tax owner update');

         OPEN get_orig_details (p_contract_id, p_conc_req_id);

         FETCH get_orig_details
         INTO lc_orig_product_name, ln_orig_pdt_id;

         CLOSE get_orig_details;

         IF lc_orig_product_name LIKE '%NON%TAX%'
         THEN
            UPDATE okc_rules_b
               SET RULE_INFORMATION1 = 'LESSEE'
             WHERE     dnz_chr_id = p_contract_id
                   AND rule_information_category = 'LATOWN';
         END IF;

         UPDATE okl_k_headers
            SET pdt_id = ln_orig_pdt_id, deal_type = 'LEASEDF'
          WHERE id = p_contract_id;

         COMMIT;

         fnd_file.put_line (fnd_file.LOG, 'Calling the abandon procedure: ');
         lc_err_msg := gc_error_message;
         lt_rev_tbl (1).chr_id := ln_rebook_chr_id;
         okl_transaction_pub.abandon_revisions (
            p_api_version       => 1.0,
            p_init_msg_list     => xxd_api.gc_true,
            x_return_status     => gc_return_status,
            x_msg_count         => gn_msg_count,
            x_msg_data          => gc_error_message,
            p_rev_tbl           => lt_rev_tbl,
            p_contract_status   => 'ABANDONED',
            p_tsu_code          => 'CANCELED');

         IF gc_return_status <> fnd_api.g_ret_sts_success
         THEN
            IF gn_msg_count > 0
            THEN
              <<err_loop>>
               FOR i IN 1 .. gn_msg_count
               LOOP
                  gc_error_message :=
                     SUBSTR (
                        gc_error_message || ':' || fnd_msg_pub.get (i, 'F'),
                        1,
                        2000);
               END LOOP err_loop;
            END IF;

            x_error_message :=
               x_error_message || lc_err_msg || gc_error_message;
            fnd_file.put_line (fnd_file.LOG, 'Exception -Abandon failedl');
            x_return_status := 'W';

            lcu_temp_dtls_rec.error_msg :=
                  'Contract Acivation Failed'
               || x_error_message
               || '- Abandon failed';
            lcu_temp_dtls_rec.status := 'E';
            lcu_temp_dtls_rec.processed_flag := 'N';
            xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);
         END IF;

         IF (x_return_status = 'W')
         THEN
            x_error_message :=
               'Error during Abandoning rebook copy - ' || gc_error_message;
            x_return_status := 'E';
         ELSE
            fnd_file.put_line (fnd_file.LOG, 'Abandoned rebook copy');
            x_return_status := 'E';
            x_error_message :=
               'Rebook failed - abandoned the rebook copy and reverted product/tax owner updates ';

            fnd_file.put_line (fnd_file.LOG, x_error_message);
         END IF;
      WHEN OTHERS
      THEN
         fnd_file.put_line (
            fnd_file.LOG,
            'Exception - Unexpected error in rebook procedure');

         OPEN get_orig_details (p_contract_id, p_conc_req_id);

         FETCH get_orig_details
         INTO lc_orig_product_name, ln_orig_pdt_id;

         CLOSE get_orig_details;

         IF lc_orig_product_name LIKE '%NON%TAX%'
         THEN
            UPDATE okc_rules_b
               SET RULE_INFORMATION1 = 'LESSEE'
             WHERE     dnz_chr_id = p_contract_id
                   AND rule_information_category = 'LATOWN';
         END IF;

         UPDATE okl_k_headers
            SET pdt_id = ln_orig_pdt_id, deal_type = 'LEASEDF'
          WHERE id = p_contract_id;

         COMMIT;

         IF get_qa_id_c%ISOPEN
         THEN
            CLOSE get_qa_id_c;
         END IF;

         fnd_file.put_line (fnd_file.LOG, 'Calling the abandon procedure: ');
         lc_err_msg := gc_error_message;
         lt_rev_tbl (1).chr_id := ln_rebook_chr_id;
         okl_transaction_pub.abandon_revisions (
            p_api_version       => 1.0,
            p_init_msg_list     => xxd_api.gc_true,
            x_return_status     => gc_return_status,
            x_msg_count         => gn_msg_count,
            x_msg_data          => gc_error_message,
            p_rev_tbl           => lt_rev_tbl,
            p_contract_status   => 'ABANDONED',
            p_tsu_code          => 'CANCELED');

         IF gc_return_status <> fnd_api.g_ret_sts_success
         THEN
            IF gn_msg_count > 0
            THEN
              <<err_loop>>
               FOR i IN 1 .. gn_msg_count
               LOOP
                  gc_error_message :=
                     SUBSTR (
                        gc_error_message || ':' || fnd_msg_pub.get (i, 'F'),
                        1,
                        2000);
               END LOOP err_loop;
            END IF;

            x_error_message :=
               x_error_message || lc_err_msg || gc_error_message;
            fnd_file.put_line (fnd_file.LOG, 'Exception -Abandon failedl');
            x_return_status := 'W';

            lcu_temp_dtls_rec.error_msg :=
                  lcu_temp_dtls_rec.error_msg
               || x_error_message
               || '- Abandon failed';
            lcu_temp_dtls_rec.status := 'E';
            lcu_temp_dtls_rec.processed_flag := 'N';
            xxd_upd_temp_tab_proc (lcu_temp_dtls_rec);
         END IF;

         IF (x_return_status = 'W')
         THEN
            x_error_message :=
               'Error during Abandoning rebook copy - ' || gc_error_message;
            x_return_status := 'E';
         ELSE
            fnd_file.put_line (fnd_file.LOG, 'Abandoned rebook copy');
            x_return_status := 'E';
            x_error_message :=
               'Rebook failed - abandoned the rebook copy and reverted product/tax owner updates ';

            fnd_file.put_line (fnd_file.LOG, x_error_message);
         END IF;

         x_error_message := x_error_message || ' - Error in rebook procedure';
         x_return_status := 'E';
   END xxd_ol_rbk_proc;
END xxd_ol_rbk_pkg;
/

SHOW ERROR

EXIT

REM ============================================================================
REM EOF: XXD_OL_RBK_PKG.pkb
REM ============================================================================