%%
[~,sequences] = fastaread("./data/rosalind_splc.txt");

%%
dna_codon_table = strsplit(strtrim(fileread("./table_codon_DNA.txt")));
dna = dna_codon_table(1:2:128); % extract every 2 items
protein = dna_codon_table(2:2:128);
conv = dictionary(dna,protein);

%% remove introns
source_seq=sequences{1};
for i = 2:length(sequences)
    source_seq = erase(source_seq, sequences{i});
end

%%
codons = reshape(source_seq,3,[])';
out = char(zeros(1,length(codons)-1));

for i = 1:length(codons)
    acid = char(lookup(conv,{codons(i,:)}));
    if strcmp(acid,"Stop")
        break
    end
    out(i)=acid;
end

disp(out)
