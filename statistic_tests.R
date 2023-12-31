#������ �������
word <- c("say", "go", "be like", "think", "zero", "be (just)", "misc")
british <- c(209, 120, 120, 123, 66, 11, 16)
canadian <- c(219, 135, 79, 27, 123, 5, 24)
dataset <- data.frame(word = word, British_English = british, Canadian_English = canadian)
head(dataset)

#�� �����, ��� ������ � ���� ��������� ���� �����������, ���� ����� ��������������� � ����������. ������, ����� ��������� qq plot, ����� ��������� � ������������ ������������� ������.
summary(dataset$British_English)
summary(dataset$Canadian_English)
qqnorm(dataset$British_English)
qqline(dataset$British_English)
#qqplot ����������, ��� ��� ����������� ����������� ������ ������������ ���������.
qqnorm(dataset$Canadian_English)
qqline(dataset$Canadian_English)
#qqplot ����������, ��� � ��� ���������� ����������� ������ ���� ������������ ���������.

#�������� � ������� ����� ������-�����, ��� ������������ ������.
shapiro.test(dataset$British_English)
#p-value ������, ��� 0.05, ������ ��� ����������� ����������� ������ ������������ ���������.
shapiro.test(dataset$Canadian_English)
#p-value ������, ��� 0.05, ������ ��� ���������� ����������� ������ ������������ ���������.

#��� ��� ����� �������� ���� ����� ����� ����� �������� ������, ��� ��������� t-test ��� ����� �������. ������ paired = TRUE, ��� ��� ����� � ��� �����������, � ��� ���������� ����������� � �������� ���������.
t.test(dataset$British_English, dataset$Canadian_English, paired = TRUE)
#������ �������������� �������� "greater", ������ ��� ��� ����� �������, ��������� ����������� ������.
t.test(dataset$British_English, dataset$Canadian_English, paired = TRUE, alternative = "greater")
#p-value ��������� ������ 0.05; ������, ��������� ����������� �������� � ���, ��� �������� ����� �������� ������ �� �������.

#�������� ������, ������������, ��� ��� ������ ������ ������������ ���������.
boxplot(dataset$British_English, dataset$Canadian_English)
#Boxplot ���������� ���, ��� ��� ����������� ����������� ������� ��������� � ������� ���������; ��� ���������� ����������� ������� ��������� ���������� ����� ������� � ������ ���������. �������� ���. ������������� ��������� ������.