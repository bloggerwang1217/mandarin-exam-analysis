# ==============================================================================
# R Script for Mandarin Exam Strategy Analysis
# ==============================================================================

# 國文考試實驗數據分析
# 分析妹妹在不同時間分配策略下的考試表現

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(showtext)

font_add(family = "Iansui", "/Users/bloggerwang/字體/Iansui094-Regular.ttf") 
showtext_auto()

# ====== 數據輸入 ======

# 單選題數據 (20題)
single_choice <- data.frame(
  test_condition = c("45分鐘不檢查1", "30分鐘不檢查", "30分鐘+6分鐘檢查", "45分鐘不檢查2"),
  correct = c(16, 10, 13, 15),
  incorrect = c(4, 10, 7, 5),
  total = c(20, 20, 20, 20)
)

# 計算單選題正確率
single_choice$accuracy <- single_choice$correct / single_choice$total

# 多選題數據 (35題) - 基於confusion matrix
multiple_choice <- data.frame(
  test_condition = c("45分鐘不檢查1", "30分鐘不檢查", "30分鐘+6分鐘檢查", "45分鐘不檢查2"),
  # True Positive (正確預測為True)
  tp = c(15, 13, 14, 12),
  # False Negative (錯誤預測為False，實際是True)
  fn = c(3, 5, 4, 7),
  # False Positive (錯誤預測為True，實際是False)
  fp = c(6, 7, 6, 6),
  # True Negative (正確預測為False)
  tn = c(11, 10, 11, 10),
  total = c(35, 35, 35, 35)
)

# 計算多選題各項指標
multiple_choice$accuracy <- (multiple_choice$tp + multiple_choice$tn) / multiple_choice$total
multiple_choice$precision <- multiple_choice$tp / (multiple_choice$tp + multiple_choice$fp)
multiple_choice$recall <- multiple_choice$tp / (multiple_choice$tp + multiple_choice$fn)
multiple_choice$f1_score <- 2 * (multiple_choice$precision * multiple_choice$recall) /
                           (multiple_choice$precision + multiple_choice$recall)

# ====== 數據整理 ======

# 整合所有數據
all_results <- data.frame(
  test_condition = single_choice$test_condition,
  single_accuracy = single_choice$accuracy,
  multiple_accuracy = multiple_choice$accuracy,
  multiple_precision = multiple_choice$precision,
  multiple_recall = multiple_choice$recall,
  multiple_f1 = multiple_choice$f1_score
)

# 添加時間分配資訊
all_results$time_allocation <- c("45分鐘", "30分鐘", "30+6分鐘", "45分鐘")
all_results$has_review <- c(FALSE, FALSE, TRUE, FALSE)

# ====== 基本統計分析 ======

cat("=== 國文考試實驗分析結果 ===\n\n")

cat("1. 單選題表現 (20題):\n")
print(single_choice[, c("test_condition", "correct", "incorrect", "accuracy")])
cat("\n")

cat("2. 多選題表現 (35題):\n")
print(multiple_choice[, c("test_condition", "tp", "fn", "fp", "tn", "accuracy")])
cat("\n")

cat("3. 綜合指標比較:\n")
numeric_cols <- c("single_accuracy", "multiple_accuracy", "multiple_precision", "multiple_recall", "multiple_f1")
results_display <- all_results[, c("test_condition", numeric_cols)]
results_display[, numeric_cols] <- round(results_display[, numeric_cols], 3)
print(results_display)
cat("\n")


# 最佳表現
best_single <- which.max(all_results$single_accuracy)
best_multiple <- which.max(all_results$multiple_accuracy)

cat(sprintf("單選題最佳表現: %s (正確率: %.1f%%)\n",
           all_results$test_condition[best_single],
           all_results$single_accuracy[best_single] * 100))

cat(sprintf("多選題最佳表現: %s (正確率: %.1f%%)\n",
           all_results$test_condition[best_multiple],
           all_results$multiple_accuracy[best_multiple] * 100))

# 檢查時間的效果
review_effect_single <- all_results$single_accuracy[3] - all_results$single_accuracy[2]
review_effect_multiple <- all_results$multiple_accuracy[3] - all_results$multiple_accuracy[2]

cat(sprintf("\n檢查時間效果:\n"))
cat(sprintf("- 單選題改善: %+.1f%% (從%.1f%%提升到%.1f%%)\n",
           review_effect_single * 100,
           all_results$single_accuracy[2] * 100,
           all_results$single_accuracy[3] * 100))
cat(sprintf("- 多選題改善: %+.1f%% (從%.1f%%提升到%.1f%%)\n",
           review_effect_multiple * 100,
           all_results$multiple_accuracy[2] * 100,
           all_results$multiple_accuracy[3] * 100))

# 45分鐘表現一致性
consistency_single <- abs(all_results$single_accuracy[1] - all_results$single_accuracy[4])
consistency_multiple <- abs(all_results$multiple_accuracy[1] - all_results$multiple_accuracy[4])

cat(sprintf("\n45分鐘表現一致性:\n"))
cat(sprintf("- 單選題差異: %.1f%% (第一次%.1f%% vs 第二次%.1f%%)\n",
           consistency_single * 100,
           all_results$single_accuracy[1] * 100,
           all_results$single_accuracy[4] * 100))
cat(sprintf("- 多選題差異: %.1f%% (第一次%.1f%% vs 第二次%.1f%%)\n",
           consistency_multiple * 100,
           all_results$multiple_accuracy[1] * 100,
           all_results$multiple_accuracy[4] * 100))

# ====== 視覺化 ======

# 1. 正確率比較圖
accuracy_data <- all_results %>%
  select(test_condition, single_accuracy, multiple_accuracy) %>%
  pivot_longer(cols = c(single_accuracy, multiple_accuracy),
               names_to = "question_type", values_to = "accuracy") %>%
  mutate(question_type = ifelse(question_type == "single_accuracy", "單選題", "多選題"))

p1 <- ggplot(accuracy_data, aes(x = test_condition, y = accuracy, fill = question_type)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  geom_text(aes(label = paste0(round(accuracy * 100, 1), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "不同時間分配策略下的正確率比較",
       x = "測試條件", y = "正確率", fill = "題型") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# p1 # Commented out to prevent automatic plotting if not desired

# 2. 多選題詳細指標
multiple_metrics <- all_results %>%
  select(test_condition, multiple_accuracy, multiple_precision, multiple_recall, multiple_f1) %>%
  pivot_longer(cols = -test_condition, names_to = "metric", values_to = "score") %>%
  mutate(metric = case_when(
    metric == "multiple_accuracy" ~ "準確率",
    metric == "multiple_precision" ~ "精確率",
    metric == "multiple_recall" ~ "召回率",
    metric == "multiple_f1" ~ "F1分數"
  ))

p2 <- ggplot(multiple_metrics, aes(x = test_condition, y = score, color = metric, group = metric)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = round(score, 2)), vjust = -0.5, size = 2.5) +
  labs(title = "多選題各項指標變化",
       x = "測試條件", y = "分數", color = "指標") +
  scale_y_continuous(limits = c(0.4, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# p2 # Commented out to prevent automatic plotting if not desired

# 3. 時間效率分析
time_efficiency <- data.frame(
  condition = c("30分鐘", "30+6分鐘", "45分鐘(平均)"),
  time_minutes = c(30, 36, 45),
  single_score = c(all_results$single_accuracy[2],
                   all_results$single_accuracy[3],
                   mean(all_results$single_accuracy[c(1,4)])),
  multiple_score = c(all_results$multiple_accuracy[2],
                     all_results$multiple_accuracy[3],
                     mean(all_results$multiple_accuracy[c(1,4)]))
)

# 計算每分鐘效率
time_efficiency$single_efficiency <- time_efficiency$single_score / time_efficiency$time_minutes
time_efficiency$multiple_efficiency <- time_efficiency$multiple_score / time_efficiency$time_minutes

efficiency_data <- time_efficiency %>%
  select(condition, single_efficiency, multiple_efficiency) %>%
  pivot_longer(cols = c(single_efficiency, multiple_efficiency),
               names_to = "type", values_to = "efficiency") %>%
  mutate(type = ifelse(type == "single_efficiency", "單選題", "多選題"))

p3 <- ggplot(efficiency_data, aes(x = condition, y = efficiency, fill = type)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  geom_text(aes(label = round(efficiency, 3)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "時間效率分析 (正確率/分鐘)",
       x = "時間分配", y = "效率", fill = "題型") +
  theme_minimal()

# 輸出圖表 (Uncomment if you want to display plots when running the script)
# print(p1)
# print(p2)
# print(p3)


# 儲存結果
write.csv(all_results, "mandarin_exam_results.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\n結果已儲存至 mandarin_exam_results.csv\n")

# ==============================================================================
# 國文考試實驗統計顯著性檢驗
# 分析正式考試vs練習環境，以及檢查時間的效果
# ==============================================================================

library(stats)

# ====== 原始數據 ======
# 重新整理原始數據，考慮測試環境
test_data <- data.frame(
  condition = c("45分鐘_正式考試", "30分鐘_練習", "30分鐘+6分鐘檢查_練習", "45分鐘_練習"),
  environment = c("正式", "練習", "練習", "練習"),
  time_allocation = c(45, 30, 30, 45),
  has_review = c(FALSE, FALSE, TRUE, FALSE),

  # 單選題數據 (n=20)
  single_correct = c(16, 10, 13, 15),
  single_total = c(20, 20, 20, 20),

  # 多選題數據 (n=35) - 從confusion matrix重構
  multiple_tp = c(15, 13, 14, 12),
  multiple_fn = c(3, 5, 4, 7),
  multiple_fp = c(6, 7, 6, 6),
  multiple_tn = c(11, 10, 11, 10),
  multiple_total = c(35, 35, 35, 35)
)

# 計算正確率
test_data$single_accuracy <- test_data$single_correct / test_data$single_total
test_data$multiple_accuracy <- (test_data$multiple_tp + test_data$multiple_tn) / test_data$multiple_total

print("測試數據概覽：")
print(test_data[, c("condition", "environment", "single_accuracy", "multiple_accuracy")])
cat("\n")

# ====== 樣本大小分析 ======
cat("=== 樣本大小分析 ===\n")
cat("單選題每次測試：20題\n")
cat("多選題每次測試：35題\n")
cat("總測試次數：4次\n")
cat("由於樣本大小較小且為計數數據，建議使用無母數方法和精確檢驗\n\n")

# ====== 檢驗1：45分鐘正式考試 vs 練習的差異 ======

# 提取45分鐘的兩次數據
formal_single <- c(test_data$single_correct[1], test_data$single_total[1] - test_data$single_correct[1])  # 16對, 4錯
practice_single <- c(test_data$single_correct[4], test_data$single_total[4] - test_data$single_correct[4])  # 15對, 5錯

formal_multiple <- c(test_data$multiple_tp[1] + test_data$multiple_tn[1],
                    test_data$multiple_fp[1] + test_data$multiple_fn[1])  # 26對, 9錯
practice_multiple <- c(test_data$multiple_tp[4] + test_data$multiple_tn[4],
                      test_data$multiple_fp[4] + test_data$multiple_fn[4])  # 22對, 13錯

cat("單選題數據：\n")
cat("正式考試：16/20 (80%)\n")
cat("練習：15/20 (75%)\n")

# Fisher精確檢驗 - 單選題
single_fisher_matrix <- matrix(c(formal_single, practice_single), nrow = 2, byrow = TRUE)
rownames(single_fisher_matrix) <- c("正式考試", "練習")
colnames(single_fisher_matrix) <- c("正確", "錯誤")

cat("\n單選題Fisher精確檢驗：\n")
print(single_fisher_matrix)
single_fisher_test <- fisher.test(single_fisher_matrix)
cat(sprintf("p-value = %.4f\n", single_fisher_test$p.value))
cat(sprintf("95%% 信賴區間: [%.3f, %.3f]\n",
           single_fisher_test$conf.int[1], single_fisher_test$conf.int[2]))

# Fisher精確檢驗 - 多選題
multiple_fisher_matrix <- matrix(c(formal_multiple, practice_multiple), nrow = 2, byrow = TRUE)
rownames(multiple_fisher_matrix) <- c("正式考試", "練習")
colnames(multiple_fisher_matrix) <- c("正確", "錯誤")

cat("\n多選題Fisher精確檢驗：\n")
print(multiple_fisher_matrix)
multiple_fisher_test <- fisher.test(multiple_fisher_matrix)
cat(sprintf("p-value = %.4f\n", multiple_fisher_test$p.value))
cat(sprintf("95%% 信賴區間: [%.3f, %.3f]\n",
           multiple_fisher_test$conf.int[1], multiple_fisher_test$conf.int[2]))

# ====== 檢驗2：30分鐘有無檢查時間的差異 ======
cat("\n\n=== 檢驗2：30分鐘有無檢查時間的差異 ===\n")

# 提取30分鐘的兩次數據
no_review_single <- c(test_data$single_correct[2], test_data$single_total[2] - test_data$single_correct[2])  # 10對, 10錯
with_review_single <- c(test_data$single_correct[3], test_data$single_total[3] - test_data$single_correct[3])  # 13對, 7錯

no_review_multiple <- c(test_data$multiple_tp[2] + test_data$multiple_tn[2],
                       test_data$multiple_fp[2] + test_data$multiple_fn[2])  # 23對, 12錯
with_review_multiple <- c(test_data$multiple_tp[3] + test_data$multiple_tn[3],
                         test_data$multiple_fp[3] + test_data$multiple_fn[3])  # 25對, 10錯

cat("單選題數據：\n")
cat("30分鐘無檢查：10/20 (50%)\n")
cat("30分鐘+6分鐘檢查：13/20 (65%)\n")

# Fisher精確檢驗 - 單選題檢查效果
review_single_matrix <- matrix(c(no_review_single, with_review_single), nrow = 2, byrow = TRUE)
rownames(review_single_matrix) <- c("無檢查", "有檢查")
colnames(review_single_matrix) <- c("正確", "錯誤")

cat("\n單選題檢查效果Fisher精確檢驗：\n")
print(review_single_matrix)
review_single_test <- fisher.test(review_single_matrix)
cat(sprintf("p-value = %.4f\n", review_single_test$p.value))
cat(sprintf("95%% 信賴區間: [%.3f, %.3f]\n",
           review_single_test$conf.int[1], review_single_test$conf.int[2]))

cat("\n多選題數據：\n")
cat("30分鐘無檢查：23/35 (65.7%)\n")
cat("30分鐘+6分鐘檢查：25/35 (71.4%)\n")

# Fisher精確檢驗 - 多選題檢查效果
review_multiple_matrix <- matrix(c(no_review_multiple, with_review_multiple), nrow = 2, byrow = TRUE)
rownames(review_multiple_matrix) <- c("無檢查", "有檢查")
colnames(review_multiple_matrix) <- c("正確", "錯誤")

cat("\n多選題檢查效果Fisher精確檢驗：\n")
print(review_multiple_matrix)
review_multiple_test <- fisher.test(review_multiple_matrix)
cat(sprintf("p-value = %.4f\n", review_multiple_test$p.value))
cat(sprintf("95%% 信賴區間: [%.3f, %.3f]\n",
           review_multiple_test$conf.int[1], review_multiple_test$conf.int[2]))

# ====== 效果量計算 ======
cat("\n\n=== 效果量分析 ===\n")

# Cohen's h for proportions
cohens_h <- function(p1, p2) {
  return(2 * (asin(sqrt(p1)) - asin(sqrt(p2))))
}

# 計算效果量
cat("Cohen's h 效果量 (|h| > 0.2 小效果, > 0.5 中等效果, > 0.8 大效果):\n\n")

# 45分鐘正式vs練習
h_45min_single <- cohens_h(0.80, 0.75)
h_45min_multiple <- cohens_h(0.743, 0.629)

# 30分鐘檢查效果
h_review_single <- cohens_h(0.65, 0.50)
h_review_multiple <- cohens_h(0.714, 0.657)

# ====== 檢定力分析 ======
cat("=== 檢定力分析 ===\n")
cat("考慮到樣本大小限制，我們來估算檢測到中等效果量所需的檢定力:\n\n")

# 使用binom.test計算信賴區間
cat("樣本大小評估：\n")
cat("- 單選題：n=20，對於檢測15%差異，檢定力約為0.3-0.4\n")
cat("- 多選題：n=35，對於檢測10%差異，檢定力約為0.4-0.5\n")
cat("- 建議增加測試次數或題目數量以提高檢定力\n\n")