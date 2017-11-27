---
title: "HW04: Clean Data Dictionary"
subtitle: "Stat 133, Fall 2017"
author: "Woo Sik (Lewis) Kim"
output: github_document
---

**Column Variables in cleanscores.csv**:

• _for information on columns HW1 - HW9, ATT, QZ1 - QZ4, EX1, and EX2, please refer to 'rawscores-dicionary.md'._

• Lab: Lab score converted from attendance points (ATT), according to a scale described in 'functions.R' (under score_lab()).

• Homework: Overall homework scores obtained by dropping the lowest HW (out of 100%), and then averaging the remaining scores.

• Quiz: Overall quiz scores obtained by dropping the lowest quiz (out of 100%), and then averaging the remaining scores.

• Test1: Values from EX1 rescaled to fit a scale of 0 to 100.

• Test2: Values from EX2 rescaled to fit a scale of 0 to 100.

• Overall: Overall course grades obtained from the following scale (out of 100%): 10% Lab score, 30% Homework score (drop lowest HW), 15% Quiz score (drop lowest quiz), 20% Test 1 score, 25% Test 2 score.

• Grade: Letter grades based on values in OVERALL, according to a scale described in 'functions.R' (under letter_grade()).
