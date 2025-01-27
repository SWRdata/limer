<?php
     /**
    This fix must be added to the file
    application/helpers/remotecontrol/remotecontrol_handle.php.
      * */



     /**
      * RPC Routine to export a survey archive (LSA).
      *
      * @access public
      * @param string $sSessionKey Auth credentials
      * @param int $iSurveyID_org Id of the survey
      * @return string|array in case of success : Base64 encoded string of the .lsa file. On failure array with error information.
      * */
     public function export_survey_archive ($sSessionKey, $iSurveyID_org)
     {
         $iSurveyID = (int) $iSurveyID_org;
         if (!$this->_checkSessionKey($sSessionKey)) {
             return array('status' => 'Invalid session key');
         }
         $aData['bFailed'] = false; // Put a var for continue
         if (!$iSurveyID) {
             $aData['sErrorMessage'] = "No survey ID has been provided. Cannot export survey";
             $aData['bFailed'] = true;
         } elseif (!Survey::model()->findByPk($iSurveyID)) {
             $aData['sErrorMessage'] = "Invalid survey ID";
             $aData['bFailed'] = true;
         } elseif (!Permission::model()->hasSurveyPermission($iSurveyID, 'surveycontent', 'export') && !Permission::model()->hasSurveyPermission($iSurveyID, 'surveycontent', 'export')) {
             $aData['sErrorMessage'] = "You don't have sufficient permissions.";
             $aData['bFailed'] = true;
         } else {
             $aExcludes = array();
             $aExcludes['dates'] = true;
             $btranslinksfields = true;
             Yii::app()->loadHelper('export');

             /* START CODE DUPLICATION
              * taken from application/controllers/admin/export.php:_exportarchive
              * This is ugly.
              * Sorry, I am just a sysadmin, no PHP dev
              * The mentioned function would somehow need to be public to actually use it
              * However, I barely understand OO programming
              *
              * I also added addToZip to export_helper.php and made it non-private
              * to be able to use it here. Ugly as well.
              *
              * Still, it works, so the concept should be clear.
              */
             $survey = Survey::model()->findByPk($iSurveyID);

             // $aSurveyInfo = getSurveyInfo($iSurveyID); // unused, even in export.php

             $sTempDir = Yii::app()->getConfig("tempdir");

             $aZIPFileName = $sTempDir.DIRECTORY_SEPARATOR.randomChars(30);
             $sLSSFileName = $sTempDir.DIRECTORY_SEPARATOR.randomChars(30);
             $sLSRFileName = $sTempDir.DIRECTORY_SEPARATOR.randomChars(30);
             $sLSTFileName = $sTempDir.DIRECTORY_SEPARATOR.randomChars(30);
             $sLSIFileName = $sTempDir.DIRECTORY_SEPARATOR.randomChars(30);

             Yii::import('application.libraries.admin.pclzip', true);
             $zip = new PclZip($aZIPFileName);

             file_put_contents($sLSSFileName, surveyGetXMLData($iSurveyID));

             addToZip($zip, $sLSSFileName, 'survey_'.$iSurveyID.'.lss');

             unlink($sLSSFileName);

             if ($survey->isActive) {
                 getXMLDataSingleTable($iSurveyID, 'survey_'.$iSurveyID, 'Responses', 'responses', $sLSRFileName, false);
                 addToZip($zip, $sLSRFileName, 'survey_'.$iSurveyID.'_responses.lsr');
                 unlink($sLSRFileName);
             }

             if ($survey->hasTokensTable) {
                 getXMLDataSingleTable($iSurveyID, 'tokens_'.$iSurveyID, 'Tokens', 'tokens', $sLSTFileName);
                 addToZip($zip, $sLSTFileName, 'survey_'.$iSurveyID.'_tokens.lst');
                 unlink($sLSTFileName);
             }

             if (isset($survey->hasTimingsTable) && $survey->hasTimingsTable == 'Y') {
                 getXMLDataSingleTable($iSurveyID, 'survey_'.$iSurveyID.'_timings', 'Timings', 'timings', $sLSIFileName);
                 addToZip($zip, $sLSIFileName, 'survey_'.$iSurveyID.'_timings.lsi');
                 unlink($sLSIFileName);
             }

             if (is_file($aZIPFileName)) {
             /* END CODE DUPLICATION */

                     $sResult = file_get_contents($aZIPFileName);
					unlink($aZIPFileName);
                 } else {
                     $aData['bFailed'] = true;
                     $aData['sErrorMessage'] = 'Error creating lsa archive';
                 }
         }
         if ($aData['bFailed']) {
             return array('status' => 'Export failed', 'error'=> $aData['sErrorMessage']);
         } else {
             return base64_encode($sResult);
         }
     }
     /**
      * RPC Routine to export a survey structure (LSS).
      *
      * @access public
      * @param string $sSessionKey Auth credentials
      * @param int $iSurveyID_org Id of the survey
      * @return string|array in case of success : Base64 encoded string of the .lss file. On failure array with error information.
      * */
     public function export_survey_structure ($sSessionKey, $iSurveyID_org)
     {
         $iSurveyID = (int) $iSurveyID_org;
         if (!$this->_checkSessionKey($sSessionKey)) {
             return array('status' => 'Invalid session key');
         }
         $aData['bFailed'] = false; // Put a var for continue
         if (!$iSurveyID) {
             $aData['sErrorMessage'] = "No survey ID has been provided. Cannot export survey";
             $aData['bFailed'] = true;
         } elseif (!Survey::model()->findByPk($iSurveyID)) {
             $aData['sErrorMessage'] = "Invalid survey ID";
             $aData['bFailed'] = true;
         } elseif (!Permission::model()->hasSurveyPermission($iSurveyID, 'surveycontent', 'export') && !Permission::model()->hasSurveyPermission($iSurveyID, 'surveycontent', 'export')) {
             $aData['sErrorMessage'] = "You don't have sufficient permissions.";
             $aData['bFailed'] = true;
         } else {
             $aExcludes = array();
             $aExcludes['dates'] = true;
             $btranslinksfields = true;
             Yii::app()->loadHelper('export');
             $exportsurveystructuredata = surveyGetXMLData($iSurveyID, $aExcludes);
             if ($exportsurveystructuredata) {
                 $sResult = $exportsurveystructuredata;
             } else {
                 $aData['bFailed'] = true;
             }
         }
         if ($aData['bFailed']) {
             return array('status' => 'Export failed', 'error'=> $aData['sErrorMessage']);
         } else {
             return base64_encode($sResult);
         }
     }


    /**
     * Obtain column ids of a surveys response table
     *
     * @access public
     *
     * @param string  $sSessionKey  Auth credentials
     * @param int     $iSurveyID    ID of the Survey
     *
     * @return array On success: array containing column names of response table
     *               On failure: array with error information
     */


    public function get_response_table_columns($sSessionKey, $iSurveyID){
        $iSurveyID = (int) $iSurveyID;
        $survey = Survey::model()->findByPk($iSurveyID);

        if ($this->_checkSessionKey($sSessionKey)) {

          if (!Permission::model()->hasSurveyPermission($iSurveyID, 'responses', 'export')) {
              return array('status' => 'No permission or survey does not exists');
          }

          if (!tableExists($survey-> responsesTableName)) {
                return array('status' => 'No Data, survey table does not exist (propably yet not activated).');
            }

          if (empty($sLanguageCode)) {
              $sLanguageCode = $survey->language;
          }
          if (is_null($aFields)) {

          $aFields = array_keys(createFieldMap($survey,  $style = 'full', $force_refresh = true, $questionid = false, $sLanguage = $sLanguageCode));
              return array($aFields);
          }

            return array('status' => "OK");
        } else {
                    return array('status' => self::INVALID_SESSION_KEY);
        }
    }


    /**
     * Copies a survey into a new survey with a desired ID
     *
     * @access public
     *
     * @param string  $sSessionKey  Auth credentials
     * @param int     $iSurveyID    ID of the Survey
     * @param int     $iDesiredSurveyId    desired ID of the new Survey
     *
     * @return array On success: OK
     *               On failure: array with error information
     */

  public function copy_survey_to($sSessionKey, $iSurveyID, $iDesiredSurveyId)
    {
        //Yii::app()->loadHelper('admin/export');
        Yii::app()->loadHelper('export');
        Yii::app()->loadHelper('admin/import');
        $iSurveyID = (int) $iSurveyID;
        $iDesiredSurveyId = (int) $iDesiredSurveyId;

        $aExcludes = array();
        $aExcludes['dates'] = true;
        $copysurveydata = surveyGetXMLData($iSurveyID, $aExcludes);

        if ($copysurveydata) {

            $aImportResults = XMLImportSurvey('', $copysurveydata, $iDesiredSurveyId = $iDesiredSurveyId);

            Permission::model()->copySurveyPermissions($iSurveyID, $aImportResults['newsid']);
            return array("status" => "ok");
      }

  }




?>
