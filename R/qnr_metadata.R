#' Extract values of JSON object
#' 
#' @param x JSON questionnaire object
#' 
#' @return 
#' 
#' @import tidyjson
#' 
#' @noRd 
extract_values <- function(
    x
) {

    tidyjson::spread_values(.x = x,
        
        # general attributes
        type = tidyjson::jstring("$type"),
        varname = tidyjson::jstring("VariableName"),
        public_key = tidyjson::jstring("PublicKey"),
        condition_expression = tidyjson::jstring("ConditionExpression"),
        hide_if_disabled = tidyjson::jlogical("HideIfDisabled"),
        
        # question attributes
        featured = tidyjson::jlogical("Featured"),
        # children = -- this is an array
        # answers = -- this is an array
        # properties = -- this is an object with named entries
            # hide_instructions
            # use_formatting
            # options_filter_expression
        question_scope = tidyjson::jnumber("QuestionScope"),
        question_text = tidyjson::jstring("QuestionText"),
        instructions = tidyjson::jstring("Instructions"),
        question_type = tidyjson::jstring("QuestionType"),
        stata_export_caption = tidyjson::jstring("StataExportCaption"),
        variable_label = tidyjson::jstring("VariableLabel"),

        # question type-specific attributes
        # single-select
        show_as_list = tidyjson::jlogical("ShowAsList"),
        # multi-select
        answer_order = tidyjson::jnumber("AnswerOrder"),
        are_answered_ordered = tidyjson::jlogical("AreAnswersOrdered"),
        yes_no_view = tidyjson::jlogical("YesNoView"),
        # single- or mult-select
        is_filtered_combo_box = tidyjson::jstring("IsFilteredCombobox"),
        linked_to_roster_id = tidyjson::jstring("LinkedToRosterId"),
        linked_filter_expression = tidyjson::jstring("LinkedFilterExpression"),        
        # date
        is_timestamp = tidyjson::jstring("IsTimestamp"),
        # numeric
        is_integer = tidyjson::jlogical("IsInteger"),
        num_decimal_places = tidyjson::jnumber("CountOfDecimalPlaces"),
        use_formatting_numeric = tidyjson::jlogical("UseFormatting"),
        # text
        mask = tidyjson::jstring("Mask"),

        # static text attributes
        text = tidyjson::jstring("Text"), # static text
        attachment_name = tidyjson::jstring("AttachmentName"),

        # variable attributes
        label_variable = tidyjson::jstring("Label"), # variable
        type_variable = tidyjson::jnumber("Type"), # variable
        name_variable = tidyjson::jstring("Name"), # variable
        expression_variable = tidyjson::jstring("Expression"), # variable
        do_not_export = tidyjson::jlogical("DoNotExport"), # variable
        
        # validation -- this is an array

        # "group" attributes
        is_flat_mode = tidyjson::jlogical("IsFlatMode"),
        is_plain_mode = tidyjson::jlogical("IsPlainMode"),
        display_mode = tidyjson::jnumber("DisplayMode"),
        enabled = tidyjson::jlogical("Enabled"), # ???
        description = tidyjson::jstring("Description"), 
        is_roster = tidyjson::jlogical("IsRoster"),
        custom_roster_title = tidyjson::jstring("CustomRosterTitle"),
        roster_size_question_id = tidyjson::jstring("RosterSizeQuestionId"),
        roster_size_source = tidyjson::jnumber("RosterSizeSource"),
        # fixed_roster_titles = --- this is an array -- "FixedRosterTitles": [],
        roster_title_question_id = tidyjson::jstring("RosterTitleQuestionId"),
        title = tidyjson::jstring("Title")

    )        
}

#' Extract information from the `properties` JSON object
#' 
#' Extract properties from these properties: `hide_instructions`, `use_formatting`, options_filter_expression
#' 
#' @import tidyjson
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @importFrom rlang .data
#' 
#' @noRd 
extract_properties <- function(x) {

    x %>%
    tidyjson::enter_object("Properties") %>% 
    tidyjson::spread_values(
        hide_instructions = tidyjson::jlogical("HideInstructions"),
        use_formatting_properties = tidyjson::jlogical("UseFormatting"),
        filter_expression = tidyjson::jstring("OptionsFilterExpression")
    ) %>%
    tibble::as_tibble() %>%
    {if (nrow(.) > 0)
        dplyr::select(., .data$public_key, .data$hide_instructions, .data$use_formatting_properties, .data$filter_expression)
    else 
        dplyr::select(., .data$public_key)
    }

}

    # TODO: confirm that this works even when validations not present
    # TODO: extend this to other nested attributes
    # TODO: figure out collect several nested attributes and combine rows without duplication (e.g., one var has validatoins, another has properties, etc.)

#' Extract information from `validation` JSON object
#' 
#' Extracts validations into rectangular format. In the JSON object, `validation` is an array, and needs to be "rectangled" with this function.
#' 
#' @param x JSON object
#' 
#' @import tidyjson
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr `%>%` select
#' 
#' @noRd 
extract_validations <- function(x) {

    x %>% 
    tidyjson::enter_object("ValidationConditions") %>% 
    tidyjson::gather_array("validations") %>% 
    tidyjson::spread_values(
        validation_expression = tidyjson::jstring("Expression"), 
        validation_message = tidyjson::jstring("Message"), 
        severity = tidyjson::jnumber("Severity") 
    ) %>% 
    tibble::as_tibble() %>%
    {if (nrow(.) > 0)
        tidyr::pivot_wider(., 
            id_cols = .data$public_key,
            names_from = c(.data$validations, .data$validations, .data$validations),
            values_from = c(.data$validation_expression, .data$validation_message, .data$severity)
        ) 
    else
        dplyr::select(., .data$public_key)
    }

}

#' Extract information from the `answers` JSON object
#' 
#' In the SuSo questionnaire, `answers` is an array that needs to be "rectangled". This function does just that. 
#' When the `answers` object has content, this function rectangularizes that content. 
#' When the `answers` object is empty, this function returns a null value.
#' 
#' @import tidyjson
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select
#' @importFrom rlang .data
#' 
#' @noRd 
extract_answers <- function(x) {

    x %>%
    tidyjson::enter_object("Answers") %>%
    tidyjson::gather_array("answers") %>%
    tidyjson::spread_values(
        answer_text = tidyjson::jstring("AnswerText"),
        answer_value = tidyjson::jstring("AnswerValue")
    ) %>%
    tibble::as_tibble() %>%
    {if (nrow(.) > 0)
        tidyr::pivot_wider(.,
            id_cols = .data$public_key,
            names_from = c(.data$answers, .data$answers),
            values_from = c(.data$answer_text, .data$answer_value))
    else 
        dplyr::select(., .data$public_key)
    }

}

#' Extract values from a level of nesting
#' 
#' In JSON format, Survey Solutions allows up to 9 levels of nesting. 
#' This family of functions navigate the JSON nodes to the desired level and extracts values at that level.
#' 
#' @param qnr_json Questionnaire JSON object
#' 
#' @importFrom tidyjson enter_object gather_array
#' @importFrom dplyr `%>%`
#' 
#' @noRd 
qnr_level_0 <- function(qnr_json) {

    qnr_json %>% 
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_0") %>%
    extract_values()

}

#' @describeIn qnr_level_0 Gets values for first level of nesting: sub-sections, rosters, questions, static text, variables, etc.
#' @noRd 
qnr_level_1 <- function(qnr_json) {

    qnr_json %>% 
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_0") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_1") %>%
    extract_values()

}

#' @describeIn Collect objects at 2nd level of nesting
#' @noRd 
qnr_level_2 <- function(qnr_json) {

    qnr_json %>% 
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_0") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_1") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_2") %>%
    extract_values()

}

#' @describeIn Collect objects at 3rd level of nesting
#' @noRd 
qnr_level_3 <- function(qnr_json) {

    qnr_json %>% 
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_0") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_1") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_2") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_3") %>%
    extract_values()

}

#' @describeIn Collect objects at 4th level of nesting
#' @noRd 
qnr_level_4 <- function(qnr_json) {

    qnr_json %>% 
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_0") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_1") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_2") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_3") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_4") %>%
    extract_values()

}

#' @describeIn Collect objects at 5th level of nesting
#' @noRd 
qnr_level_5 <- function(qnr_json) {

    qnr_json %>% 
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_0") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_1") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_2") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_3") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_4") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_5") %>%
    extract_values()

}

#' @describeIn Collect objects at 6th level of nesting
#' @noRd 
qnr_level_6 <- function(qnr_json) {

    qnr_json %>% 
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_0") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_1") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_2") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_3") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_4") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_5") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_6") %>%
    extract_values()

}

#' @describeIn Collect objects at 7th level of nesting
#' @noRd 
qnr_level_7 <- function(qnr_json) {

    qnr_json %>% 
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_0") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_1") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_2") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_3") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_4") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_5") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_6") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_7") %>%
    extract_values()

}

#' @describeIn Collect objects at 8th level of nesting
#' @noRd 
qnr_level_8 <- function(qnr_json) {

    qnr_json %>% 
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_0") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_1") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_2") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_3") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_4") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_5") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_6") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_7") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_8") %>%
    extract_values()

}    

#' @describeIn Collect objects at 9th level of nesting
#' @noRd 
qnr_level_9 <- function(qnr_json) {

    qnr_json %>% 
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_0") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_1") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_2") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_3") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_4") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_5") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_6") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_7") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_8") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_9") %>%
    extract_values()

}

#' @describeIn Collect objects at 10th level of nesting
#' @noRd 
qnr_level_10 <- function(qnr_json) {

    qnr_json %>% 
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_0") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_1") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_2") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_3") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_4") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_5") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_6") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_7") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_8") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_9") %>%
    tidyjson::enter_object("Children") %>% tidyjson::gather_array("l_10") %>%
    extract_values()

}

#' Extract all JSON attributes in a level of nesting
#' 
#' Extract attributes, properties, answers, and validations. 
#' Regular attributes for the level are extracted using the appropriate `qnr_level_*` function. 
#' Attributes in named arrays are extracted using the appropriate `extract_*` function.
#' 
#' @param qnr_json JSON questionnaire object
#' @param fun Function for extracting attributes: `qnr_level_*`
#' 
#' @importFrom rlang enexpr eval_bare
#' @importFrom dplyr `%>%` left_join
#' @importFrom tibble as_tibble
#' 
#' @noRd 
extract_level <- function(
    qnr_json,
    fun
) {

    # attributes on level
    my_expr <- rlang::enexpr(fun)
    lvl_attributes <- rlang::eval_bare(my_expr)

    # properties
    lvl_properties <- lvl_attributes %>% extract_properties()

    # answers
    lvl_answers <- lvl_attributes %>% extract_answers()

    # validation conditions
    lvl_validations <- lvl_attributes %>% extract_validations()

    # combine all attributes to form extracted attributes from that level
    level <- lvl_attributes %>%
        {if (nrow(lvl_properties) > 0 ) dplyr::left_join(., lvl_properties, by = "public_key") else .} %>%
        {if (nrow(lvl_answers) > 0) dplyr::left_join(., lvl_answers, by = "public_key") else .} %>%
        {if (nrow(lvl_validations) > 0) dplyr::left_join(., lvl_validations, by = "public_key") else .} %>%      
        tibble::as_tibble()
    
    return(level)

}

#' Parses JSON questionnaire file into rectangular data frame
#'
#' Transforms the information found in the JSON questionnaire file, `document.json`, into a simpler rectangular file. 
#' The JSON file provides a (deeply) nested representation of the questionnaire. This function returns a rectangular data frame in the following format: 
#' 
#' - Rows: objects in the questionnaire (e.g., sections, sub-sections, rosters, questions, variables, static text, etc.)
#' - Columns: attributes. 
#' 
#' This format facilitates common needs such as: 
#' 
#' - Collecting all the validations
#' - Compiling keys and values for questions (e.g., PublicKey for questions to understand the paradata AnswerRemove event)
#' 
#' @param path Complete file path to the questionnaire JSON file, typically named `document.json`
#' 
#' @return Data frame that contains all attributes of all objects in the questionnaire.
#'
#' @importFrom dplyr bind_rows
#' @importFrom tidyjson read_json
#'
#' @export
parse_questionnaire <- function(path) {

    # injest questionnaire JSON file    
    qnr_json <- tidyjson::read_json(path = path)

    # level 0
    level_0 <- extract_level(
                    qnr_json = qnr_json, 
                    fun = qnr_level_0(qnr_json = qnr_json)
                )

    # level 1
    level_1 <- extract_level(
                    qnr_json = qnr_json, 
                    fun = qnr_level_1(qnr_json = qnr_json)
                )

    # level 2
    level_2 <- extract_level(
                    qnr_json = qnr_json, 
                    fun = qnr_level_2(qnr_json = qnr_json)
                )

    # level 3
    level_3 <- extract_level(
                    qnr_json = qnr_json, 
                    fun = qnr_level_3(qnr_json = qnr_json)
                )

    # level 4
    level_4 <- extract_level(
                    qnr_json = qnr_json, 
                    fun = qnr_level_4(qnr_json = qnr_json)
                )

    # level 5
    level_5 <- extract_level(
                    qnr_json = qnr_json, 
                    fun = qnr_level_5(qnr_json = qnr_json)
                )

    # level 6
    level_6 <- extract_level(
                    qnr_json = qnr_json, 
                    fun = qnr_level_6(qnr_json = qnr_json)
                )

    # level 7
    level_7 <- extract_level(
                    qnr_json = qnr_json, 
                    fun = qnr_level_7(qnr_json = qnr_json)
                )

    # level 8
    level_8 <- extract_level(
                    qnr_json = qnr_json, 
                    fun = qnr_level_8(qnr_json = qnr_json)
                )

    # level 8
    level_8 <- extract_level(
                    qnr_json = qnr_json, 
                    fun = qnr_level_8(qnr_json = qnr_json)
                )

    # level 9
    level_9 <- extract_level(
                    qnr_json = qnr_json, 
                    fun = qnr_level_9(qnr_json = qnr_json)
                )

    # level 10
    level_10 <- extract_level(
                    qnr_json = qnr_json, 
                    fun = qnr_level_10(qnr_json = qnr_json)
                )    

    qnr_df <- dplyr::bind_rows(
        level_0,
        level_1,
        level_2,
        level_3,
        level_4,
        level_5,
        level_6,
        level_7,
        level_8,
        level_9,
        level_10)

    return(qnr_df)

}

#' Extract answer options
#' 
#' Obtain the answer options for a target variable as a set of name-value pairs in labelled numeric vector.
#' 
#' @param qnr_df Data frame returned from `parse_questionnaire`
#' @param varname Variable name. Bare name of variable whose answer options to extract.
#' @param to_exclude Numeric vector. Code of answer options to exclude.
#' 
#' @return Named numeric vector. Values are answer codes. Names are answer labels.
#' 
#' @importFrom dplyr `%>%` filter select starts_with
#' @importFrom rlang .data as_label expr
#' @importFrom tidyr pivot_longer
#' @importFrom stats setNames
#' 
#' @export 
extract_answer_options <- function(
    qnr_df,
    varname,
    to_exclude = NULL
) {

    # select answer and value columns from questionnaire data frame
    answer_options <- qnr_df %>%
        # find attributes for variable of interest
        dplyr::filter(varname == rlang::as_label(rlang::expr({{varname}}))) %>%
        # select columns that capture answer text and values
        dplyr::select(dplyr::starts_with("answer_text"), dplyr::starts_with("answer_value")) %>%
        # pivot so that option text and values occupy their own columns
        tidyr::pivot_longer(
            cols = tidyr::everything(),
            names_to = c(".value", "index"),            # create 1 column per each value found in pattern, named what is found in expression
            names_pattern = "answer_([a-z]+)_([0-9]+)", # variable names: `answer_text_{n}` and `answer_value_{n}`
            values_drop_na = TRUE                       # drop rows that contain only NAs--that is, empty answer_text and answer_value columns
        ) %>%
        # exclude label values
        {if (length(to_exclude) >= 1) filter(., !.data$value %in% to_exclude) else .}

    # create named vector, where:
    # - names are variable names
    # - values are text 
    column_labels <- stats::setNames(
        object = answer_options$text, 
        nm = paste0(
                # construct variable name from: variable stub name, separator, and value
                as_label(expr({{varname}})),   # variable stub name
                "__",           # separator
                # value
                # replace negative value (-) with "n"
                stringr::str_replace(answer_options$value, "-", "n")
            )
    )

    return(column_labels)

}
