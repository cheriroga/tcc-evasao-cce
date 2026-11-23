# pdflatex: o template usa logos em PNG e a declaração de IA entra como PDF.
$pdf_mode = 1;
# \includepdf descarta campos de formulário e assinaturas; pdftocairo fixa-os na página.
# O PDF assinado traz as caixas marcadas (/V /Yes) com aparência vazia, e o
# poppler não desenha a marca; numa cópia temporária a aparência vazia vira
# espaços (mesmo tamanho, o original assinado não é tocado) e o poppler a gera.
{
  local $/;
  open(my $in, '<:raw', 'declaracao_de_uso_de_ai.pdf') or die "sem a declaração de uso de IA\n";
  my $pdf = <$in>; close $in;
  $pdf =~ s{(/AP << /N << /Yes <<>>>>>>)}{' ' x length($1)}ge;
  open(my $out, '>:raw', 'declaracao_de_uso_de_ai_tmp.pdf') or die;
  print $out $pdf; close $out;
}
system('pdftocairo -pdf declaracao_de_uso_de_ai_tmp.pdf declaracao_de_uso_de_ai_flat.pdf') == 0
  or die "pdftocairo falhou na declaração de uso de IA\n";
unlink 'declaracao_de_uso_de_ai_tmp.pdf';
